!>
!! \file ccpr_dust_common_mod.F90
!! \brief Contains module ccpr_dust_common_mod
!!
!! \ingroup catchem_dust_process
!!
!! \author Barry Baker
!! \date 05/2024
!!!>
module CCPr_Megan_Common_Mod
   use precision_mod, only: fp, ZERO
   use Error_Mod
   implicit none
   private

   private :: GET_MEGAN_PARAMS
   private :: GET_GAMMA_PAR_PCEEA
   private :: GET_GAMMA_T_LI
   private :: GET_GAMMA_T_LD
   private :: GET_GAMMA_LAI
   private :: GET_GAMMA_AGE
   private :: GET_GAMMA_SM
   private :: CALC_NORM_FAC
   private :: SOLAR_ANGLE
   private :: GET_GAMMA_CO2
   public  :: MeganStateType

   !> \brief Type for CATCHem Megan Process
   !!
   !! \details Contains all the information needed to run the CATChem Megan Process
   !!
   !! This type contains the following variables:
   !! - Activate : Activate Process (True/False)
   !! - nMeganSpecies : Number of megan processes
   !! - MeganSpeciesIndex : Index of megan species
   !! - SpcIDs : CATChem species IDs
   !! - CO2Inhib      : CO2 inhibition for isoprene Option [true/false]
   !! - CO2conc       : CO2 concentration [ppmv]
   !! - ISOPscale     : factors to scale isoprene emissions
   !! - ISOPtoSOAP    : isoprene convertion factor to SOAP
   !! - ISOPtoSOAS    : isoprene convertion factor to SOAS
   !! - MONOtoSOAP    : monoterpene convertion factor to SOAP
   !! - MONOtoSOAS    : monoterpene convertion factor to SOAS
   !! - TERPtoSOAP    : other terpene convertion factor to SOAP
   !! - TERPtoSOAS    : other terpene convertion factor to SOAS
   !! - TotalEmission : Total Emission          [ug/m^2/s]
   !! - EmissionPerSpecies : Emission Rate per dust species  [ug/m^2/s]
   !!
   !! \ingroup catchem_megan_process
   !!!>
   TYPE :: MeganStateType
      ! Generic Variables for Every Process
      Logical                         :: Activate                !< Activate Process (True/False)
      INTEGER                         :: nMeganSpecies            !< Number of megan processes
      INTEGER, ALLOCATABLE            :: MeganSpeciesIndex(:)     !< Index of dust species
      INTEGER, ALLOCATABLE            :: SpcIDs(:)               !< CATChem species IDs

      ! Process Specific Parameters
      REAL(fp), ALLOCATABLE           :: TotalEmission           !< Total emission          [kg/m^2/s]
      REAL(fp), ALLOCATABLE           :: EmissionPerSpecies(:)   !< Emission per species    [kg/m^2/s]

      ! Scheme Options
      Logical                         :: CO2Inhib                !< CO2 inhibition for isoprene Option [True/False]
      REAL(fp)                        :: CO2conc                 !< CO2 concentration [ppmv]
      REAL(fp)                        :: ISOPscale               !< factors to scale isoprene emissions 
      REAL(fp)                        :: ISOPtoSOAP              !< isoprene convertion factor to SOAP
      REAL(fp)                        :: ISOPtoSOAS              !< isoprene convertion factor to SOAS
      REAL(fp)                        :: MONOtoSOAP              !< monoterpene convertion factor to SOAP
      REAL(fp)                        :: MONOtoSOAS              !< monoterpene convertion factor to SOAS 
      REAL(fp)                        :: TERPtoSOAP              !< other terpenes convertion factor to SOAP 
      REAL(fp)                        :: TERPtoSOAS              !< other terpenes convertion factor to SOAS 


      !=================================================================
      ! Module specific variables/arrays/data pointers come below
      !=================================================================

   END TYPE MeganStateType

contains
   !>
   !! \brief Computes the soil moisture attenuation factor for dust emission
   !!
   !! Fecan, F., Marticorena, B., and Bergametti, G.: Parametrization of the increase of the aeolian
   !! erosion threshold wind friction velocity due to soil moisture for arid and semi-arid areas,
   !! Ann. Geophys., 17, 149–157, https://doi.org/10.1007/s00585-999-0149-7, 1999.
   !!
   !! \param clay Fractional clay content
   !! \param sand Fractional sand content
   !! \param volumetric_soil_moisture Volumetric soil moisture
   !! \param H Soil moisture attenuation factor for dust emission
   !!
   !! \ingroup catchem_megan_process
   !!!>
   subroutine Calc_Sun_Frac( LAI, Sinbeta, Distgauss, SunFrac)
      IMPLICIT NONE
      ! Parameters
      !-----------
      real(fp), intent(in)  :: LAI         !< leaf area index
      real(fp), intent(in)  :: Sinbeta     !< 
      real(fp), intent(in)  :: Distgauss   !< 
      real(fp), intent(out) :: SunFrac     !< Activity factor for the light-independent fraction of emissions

      ! Local Variables
      !----------------
      real(fp), parameter :: Cluster = 0.9 !< Standard reference temperature [K]
      real(fp), parameter :: CANTRAN = 0.2 !< 
      real(fp) :: Kb, LAIadj, LAIdepth     !< 

      !--------------------------------------------
      ! main function
      !--------------------------------------------
      Kb = Cluster * 0.5 / Sinbeta
      LAIadj = LAI / ( 1 - CANTRAN )
      LAIdepth   = LAIadj  * Distgauss

      if ((Sinbeta  > 0.002) .AND. (LAIadj  > 0.001)) then
        SunFrac = EXP(-Kb * LAIdepth)
      else
        SunFrac = 0.2
      endif
      return

   end subroutine Calc_Sun_Frac

   !>
   !! \returns the emission parameters for each MEGAN compound needed to compute emissions.
   !!
   !! Guenther et al, (GMD 2012) and associated MEGANv2.1 source code
   !!
   !! \param CPD,   
   !! \param BTA,   LIDF,  C_T1,  C_EO, A_NEW, A_GRO, A_MAT, A_OLD, BI_DIR
   !!
   !! \ingroup catchem_megan_process
   !!!>
   subroutine GET_MEGAN_PARAMS( CPD,   BTA,   LIDF,  C_T1,  C_EO, A_NEW, A_GRO, A_MAT, A_OLD, BI_DIR, RC)
      
      ! Uses
      use precision_mod, only : fp, ZERO
      Use Error_Mod,     Only : CC_SUCCESS, CC_FAILURE, CC_Error
      IMPLICIT NONE
      ! input Parameters
      character(LEN=*), intent(in) :: CPD       ! Compound name
      ! input/output Parameters
      real(fp), intent(inout)  :: BTA      !< Beta coefficient for temperature activity factor for light-independent fraction
                                                          
      real(fp), intent(inout)  :: LIDF     !< Light-dependent fraction of emissions
      real(fp), intent(inout)  :: C_T1     !< CT1 parameter for temperature activity factor for light-dependent fraction
      real(fp), intent(inout)  :: C_EO     !< Ceo parameter for temperature activity factor for light-dependent fraction
      real(fp), intent(inout)  :: A_NEW    !< Relative emission factor (new leaves)
      real(fp), intent(inout)  :: A_GRO    !< Relative emission factor (growing leaves)
      real(fp), intent(inout)  :: A_MAT    !< Relative emission factor (mature leaves)
      real(fp), intent(inout)  :: A_OLD    !< Relative emission factor (old leaves)
      logical,  intent(inout)) :: BI_DIR   !< Logical flag to indicate bidirectional exchange
      integer,  intent(out)    :: RC       !< Success or Failure

      !local variables
      character(len=255)       :: MSG, thisLoc

      !=================================================================
      ! GET_MEGAN_PARAMS begins here!
      !=================================================================

      ! Initialize values
      BTA    = 0.0_fp
      LIDF   = 0.0_fp
      C_T1   = 0.0_fp
      C_EO   = 0.0_fp
      A_NEW  = 0.0_fp
      A_GRO  = 0.0_fp
      A_MAT  = 0.0_fp
      A_OLD  = 0.0_fp
      BI_DIR = .FALSE.

      ! ----------------------------------------------------------------
      ! Note that not all the above compounds are used in standard chemistry
      ! simulations, but they are provided here for future incorporation or
      ! specialized applications. More compounds can be added as needed
      ! by adding the corresponding CPD name and the appropriate paramaters.
      !
      ! Values are from Table 4 in Guenther et al., 2012
      ! ----------------------------------------------------------------

      ! Isoprene, MBO
      IF ( TRIM(CPD) == 'ISOP' .OR. &
           TRIM(CPD) == 'MBOX' ) THEN
         BTA    = 0.13_fp  ! Not actually used for ISOP, MBO
         LIDF   = 1.0_fp
         C_T1   = 95.0_fp
         C_EO   = 2.0_fp
         A_NEW  = 0.05_fp
         A_GRO  = 0.6_fp
         A_MAT  = 1.0_fp
         A_OLD  = 0.9_fp
         BI_DIR = .FALSE.

      ! Myrcene, sabinene, alpha-pinene
      ELSE IF ( TRIM(CPD) == 'MYRC' .OR. &
                TRIM(CPD) == 'SABI' .OR. &
                TRIM(CPD) == 'APIN' ) THEN
         BTA    = 0.10_fp
         LIDF   = 0.6_fp
         C_T1   = 80.0_fp
         C_EO   = 1.83_fp
         A_NEW  = 2.0_fp
         A_GRO  = 1.8_fp
         A_MAT  = 1.0_fp
         A_OLD  = 1.05_fp
         BI_DIR = .FALSE.

      ! Limonene, 3-carene, beta-pinene
      ELSE IF ( TRIM(CPD) == 'LIMO' .OR. &
                TRIM(CPD) == 'CARE' .OR. &
                TRIM(CPD) == 'BPIN' ) THEN
         BTA    = 0.10_fp
         LIDF   = 0.2_fp
         C_T1   = 80.0_fp
         C_EO   = 1.83_fp
         A_NEW  = 2.0_fp
         A_GRO  = 1.8_fp
         A_MAT  = 1.0_fp
         A_OLD  = 1.05_fp
         BI_DIR = .FALSE.

      ! t-beta-ocimene
      ELSE IF ( TRIM(CPD) == 'OCIM' ) THEN
         BTA    = 0.10_fp
         LIDF   = 0.8_fp
         C_T1   = 80.0_fp
         C_EO   = 1.83_fp
         A_NEW  = 2.0_fp
         A_GRO  = 1.8_fp
         A_MAT  = 1.0_fp
         A_OLD  = 1.05_fp
         BI_DIR = .FALSE.

      ! Other monoterpenes (lumped)
      ELSE IF ( TRIM(CPD) == 'OMON' ) THEN
         BTA    = 0.10_fp
         LIDF   = 0.4_fp
         C_T1   = 80.0_fp
         C_EO   = 1.83_fp
         A_NEW  = 2.0_fp
         A_GRO  = 1.8_fp
         A_MAT  = 1.0_fp
         A_OLD  = 1.05_fp
         BI_DIR = .FALSE.

      ! Methanol
      ELSE IF ( TRIM(CPD) == 'MOH' ) THEN
         BTA    = 0.08_fp
         LIDF   = 0.8_fp
         C_T1   = 60.0_fp
         C_EO   = 1.6_fp
         A_NEW  = 3.5_fp
         A_GRO  = 3.0_fp
         A_MAT  = 1.0_fp
         A_OLD  = 1.2_fp
         BI_DIR = .FALSE.

      ! Acetone
      ELSE IF ( TRIM(CPD) == 'ACET' ) THEN
         BTA    = 0.1_fp
         LIDF   = 0.2_fp
         C_T1   = 80.0_fp
         C_EO   = 1.83_fp
         A_NEW  = 1.0_fp
         A_GRO  = 1.0_fp
         A_MAT  = 1.0_fp
         A_OLD  = 1.0_fp
         BI_DIR = .FALSE.

      ! Bidirectional VOC: Ethanol, formaldehyde, acetaldehyde, formic acid,
      ! acetic acid
      ELSE IF ( TRIM(CPD) == 'EOH'  .OR. &
                TRIM(CPD) == 'CH2O' .OR. &
                TRIM(CPD) == 'ALD2' .OR. &
                TRIM(CPD) == 'FAXX' .OR. &
                TRIM(CPD) == 'AAXX' ) THEN
         BTA    = 0.13_fp
         LIDF   = 0.8_fp
         C_T1   = 95.0_fp
         C_EO   = 2.0_fp
         A_NEW  = 1.0_fp
         A_GRO  = 1.0_fp
         A_MAT  = 1.0_fp
         A_OLD  = 1.0_fp
         BI_DIR = .TRUE.

      ! Stress VOCs: ethene, toluene, HCN
      ! There are others species in this category but none are currently
      ! used in GEOS-Chem
      ELSE IF ( TRIM(CPD) == 'C2H4' .OR. &
                TRIM(CPD) == 'TOLU' .OR. &
                TRIM(CPD) == 'HCNX' ) THEN
         BTA    = 0.1_fp
         LIDF   = 0.8_fp
         C_T1   = 80.0_fp
         C_EO   = 1.83_fp
         A_NEW  = 1.0_fp
         A_GRO  = 1.0_fp
         A_MAT  = 1.0_fp
         A_OLD  = 1.0_fp
         BI_DIR = .FALSE.

      ! Other VOCs: >C2 alkenes
      ! This includes propene, butene and very minor contribution from
      ! larger alkenes
      ELSE IF ( TRIM(CPD) == 'PRPE' ) THEN
         BTA    = 0.1_fp
         LIDF   = 0.2_fp
         C_T1   = 80.0_fp
         C_EO   = 1.83_fp
         A_NEW  = 1.0_fp
         A_GRO  = 1.0_fp
         A_MAT  = 1.0_fp
         A_OLD  = 1.0_fp
         BI_DIR = .FALSE.
      
      ! SOAupdate: Sesquiterpenes hotp 3/2/10
      ! alpha-Farnesene, beta-Caryophyllene, other sesquiterpenes
      ELSE IF ( TRIM(CPD) == 'FARN' .OR. &
                TRIM(CPD) == 'BCAR' .OR. &
                TRIM(CPD) == 'OSQT' ) THEN
         BTA    = 0.17_fp
         LIDF   = 0.5_fp
         C_T1   = 130.0_fp
         C_EO   = 2.37_fp
         A_NEW  = 0.4_fp
         A_GRO  = 0.6_fp
         A_MAT  = 1.0_fp
         A_OLD  = 0.95_fp
         BI_DIR = .FALSE.

      ! Calls for any other MEGAN compounds (e.g. sesquiterpenes, etc.) can
      ! be added following the above format based on the parameters in
      ! Guenther 2012 or the MEGAN source code.
      ELSE
         RC = CC_FAILURE
         MSG = 'Invalid compound name'
         thisLoc = ' -> at CCPr_Megan_Common (in process/megan/ccpr_megan_common_mod.F90)'
         call CC_Error( MSG, RC , thisLoc)
         return

      ENDIF
      return

   end subroutine GET_MEGAN_PARAMS

   !>
   !! \brief Computes the PCEEA gamma activity factor with sensitivity to light
   !!
   !! References:
   !! Guenther et al, 2006; Guenther et al, 2007, MEGAN v2.1 user guide
   !! Code was taken & adapted directly from the MEGAN v2.1 source code.
   !!
   !! \param Q_DIR_2, Q_DIFF_2
   !! \param PARDR_AVG_SIM, PARDF_AVG_SIM
   !! \param LAT, DOY, LocalHour
   !! \param D2RAD, RAD2D
   !!
   !! \ingroup catchem_megan_process
   !!!>
   subroutine GET_GAMMA_PAR_PCEEA(Q_DIR_2, Q_DIFF_2, PARDR_AVG_SIM, PARDF_AVG_SIM,  &
                                  LAT, DOY, LocalHour, D2RAD, RAD2D, GAMMA_P_PCEEA)
     
      IMPLICIT NONE
      ! Parameters
      real(fp),  intent(in)  :: Q_DIR_2           !< Direct PAR [umol/m2/s]
      real(fp),  intent(in)  :: Q_DIFF_2          !< Diffuse PAR [umol/m2/s]
      real(fp),  intent(in)  :: PARDR_AVG_SIM     !< Avg direct PAR [W/m2]
      real(fp),  intent(in)  :: PARDF_AVG_SIM     !< Avg diffuse PAR [W/m2]
      real(fp),  intent(in)  :: LAT               !< Note: this may need a new funtion and put in local varaible
      integer,   intent(in)  :: DOY               !< Note: this may need a new funtion and put in local varaible
      real(fp),  intent(in)  :: LocalHour         !< Note: this may need a new funtion and put in local varaible
      real(fp),  intent(in)  :: D2RAD, RAD2D      !< Note: this could be put in constants
      real(fp),  intent(out) :: GAMMA_P_PCEEA     !< GAMMA factor for light

      ! Local Variables
      real(fp) :: mmPARDR_DAILY      !< 
      real(fp) :: mmPARDF_DAILY      !< 
      real(fp) :: PAC_DAILY, PAC_INSTANT, C_PPFD
      real(fp) :: PTOA, PHI
      real(fp) :: BETA,   SINbeta
      real(fp) :: AAA, BBB
      !real(fp) :: LocalHour, LAT
      !integer  :: DOY
      !integer  :: RC

      ! Constants
      !real(fp), parameter :: mmd = 3.4         !< median mass diameter [microns]

      !-----------------------------------------------------
      ! Compute GAMMA_PAR_PCEEA
      !-----------------------------------------------------
      ! Initialize
      C_PPFD = 0.0_fp
      PTOA   = 0.0_fp
      
      ! Convert past light conditions to micromol/m2/s (!!!Do not use)
      mmPARDR_DAILY = PARDR_AVG_SIM  !* WM2_TO_UMOLM2S
      mmPARDF_DAILY = PARDF_AVG_SIM  !* WM2_TO_UMOLM2S

      ! Work out the light at the top of the canopy.
      PAC_DAILY    = mmPARDR_DAILY + mmPARDF_DAILY
      PAC_INSTANT  = Q_DIR_2       +  Q_DIFF_2

      ! Get latitude
      !LAT = HcoState%Grid%YMID%Val(I,J)

      ! Get day of year, local-time and latitude
      ! TODO: Evaluate RC?
      !CALL HcoClock_Get( HcoState%Clock, cDOY = DOY, RC=RC )
      !CALL HcoClock_GetLocal( HcoState, I, J, cH = LocalHour, RC=RC )

      ! Get solar elevation angle
      SINbeta =  SOLAR_ANGLE( DOY, LocalHour, LAT, D2RAD )
      BETA    =  ASIN( SINbeta ) * RAD2D

      IF ( SINbeta < 0.0_fp ) THEN

         GAMMA_P_PCEEA = 0.0_fp

      ELSEIF ( SINbeta > 0.0_fp ) THEN

         ! PPFD at top of atmosphere
         PTOA    = 3000.0_fp + 99.0_fp * &
                  COS( 2._fp * 3.14159265358979323_fp * &
                  ( DOY - 10.0_fp ) / 365.0_fp )

         ! Above canopy transmission
         PHI     = PAC_INSTANT / ( SINbeta * PTOA )

         ! Work out gamma P
         BBB     = 1.0_fp + 0.0005_fp *( PAC_DAILY - 400.0_fp )
         AAA     = ( 2.46_fp * BBB * PHI ) - ( 0.9_fp * PHI**2 )

         GAMMA_P_PCEEA = SINbeta * AAA

      ENDIF

      ! Screen unforced errors. IF solar elevation angle is
      ! less than 1 THEN gamma_p can not be greater than 0.1.
      IF ( BETA < 1.0_fp .AND. GAMMA_P_PCEEA > 0.1_fp ) THEN
         GAMMA_P_PCEEA  = 0.0_fp
      ENDIF

      ! Prevent negative values
      GAMMA_P_PCEEA = MAX( GAMMA_P_PCEEA , 0.0_fp )
      
      return
   end subroutine GET_GAMMA_PAR_PCEEA

   !>
   !! \brief computes the local solar angle for a given day of year, latitude and longitude (or local time).
   !!
   !! References:
   !! (1 ) Guenther et al, 2006
   !! (2 ) Guenther et al, MEGAN v2.1 user mannual 2007-09
   !! This code was taken directly from the MEGAN v2.1 source code
   !!
   !! \param DOY, SHOUR
   !! \param LAT
   !! \param D2RAD
   !!
   !! \ingroup catchem_megan_process
   !!!>
   subroutine SOLAR_ANGLE(DOY, SHOUR, LAT, D2RAD, SINbeta)

      IMPLICIT NONE
      ! Parameters
      integer,  intent(in)    :: DOY      !< Day of year 
      real(fp), intent(in)    :: SHOUR    !< Local time
      real(fp), intent(in)    :: LAT      !< Latitude
      real(fp), intent(in)    :: D2RAD    !< Degree to radiance
      real(fp), intent(out)   :: SINbeta  !< Sin of the local solar angle


      ! local variable
      real(fp) :: sindelta, cosdelta, A, B

      ! Calculation of sin beta
      sindelta = -SIN( 0.40907_fp ) * COS( 6.28_fp * ( DOY + 10 ) / 365 )

      cosdelta = (1-sindelta**2.0_fp)**0.5_fp

      A = SIN( LAT * D2RAD ) * sindelta
      B = COS( LAT * D2RAD ) * cosdelta

      SINbeta = A + B * COS( 2.0_fp * PI * ( SHOUR-12 )/24 )

      return
   end subroutine SOLAR_ANGLE

   !>
   !! \brief Computes Draxler Hoirizontal Flux
   !!
   !! Draxler, R.R, D.A. Gillette, J.S. Kirkpatrick, and J. Heller (2001),
   !! Estimating PM10 air concentrations from dust storms in Iraq, Kuwait,
   !! and Saudi Arabia, Atm. Environ, 35: 4315-4330.
   !! https://doi.org/10.1016/S1352-2310(01)00159-5
   !!
   !! \param ustar friction velocity
   !! \param ustar_threshold dry threshold friction velocity
   !! \param R Drag partition
   !! \param H Soil Moisture Attenuation Factor
   !! \param HorizFlux Horizontal Mass Flux
   !!
   !!
   !! \ingroup catchem_dust_process
   !!!>
   subroutine Draxler_HorizFlux(ustar, ustar_threshold, R, H, HorizFlux)
      IMPLICIT NONE
      ! Parameters
      real(fp), intent(in)  :: ustar            !< friction velocity [m/s]
      real(fp), intent(in)  :: ustar_threshold  !< dry threshold friction velocity [m/s]
      real(fp), intent(in)  :: R                !< Drag partition (0-1)
      real(fp), intent(in)  :: H                !< Soil Moisture Attenuation Factor
      real(fp), intent(inout) :: HorizFlux      !< Horizontal Mass Flux [kg/m2/s]

      ! Local Variables
      !----------------
      real(fp) :: u_ts    !< Modified threshold friction velocity

      ! Initialize
      !-----------
      HorizFlux = ZERO

      !--------------------------------------------
      ! Compute Draxler Horizontal Flux
      !--------------------------------------------
      u_ts = ustar_threshold * H / R

      if (ustar >= ustar_threshold) then
         HorizFlux = max(0._fp ,(ustar * R) ** 3.0_fp * (1.0_fp - ( u_ts / ustar ) ** 2.0_fp))
      endif


      return
   end subroutine Draxler_HorizFlux

   !>
   !! \brief Computes Kawamura Hoirizontal Flux
   !!
   !! Kawamura, R., 1951. Study on sand movement by wind. Report, 5(3), pp.95-112.
   !!
   !! Webb, N., Chappell, A., LeGrand, S., Ziegler, N., Edwards, B. 2020.
   !! A note on the use of drag partition in aeolian transport models.
   !! Aeolian Research. 42:100560. https://doi.org/10.1016/j.aeolia.2019.100560.
   !!
   !! \param ustar friction velocity
   !! \param ustar_threshold dry threshold friction velocity
   !! \param R Drag partition
   !! \param H Soil Moisture Attenuation Factor
   !! \param HorizFlux Horizontal Mass Flux
   !!
   !! \ingroup catchem_dust_process
   !!!>
   subroutine Kawamura_HorizFlux(ustar, ustar_threshold, R, H, HorizFlux)
      IMPLICIT NONE
      ! Parameters
      real(fp), intent(in)  :: ustar           !< friction velocity [m/s]
      real(fp), intent(in)  :: ustar_threshold !< dry threshold friction velocity [m/s]
      real(fp), intent(in)  :: R               !< Drag partition (0-1)
      real(fp), intent(in)  :: H               !< Soil Moisture Attenuation Factor
      real(fp), intent(inout) :: HorizFlux     !<

      ! Local Variables
      real(fp) :: u_ts !< Modified threshold friction velocity

      ! Initialize
      HorizFlux = ZERO

      !--------------------------------------------
      ! Compute Kawamura Horizontal Flux
      !--------------------------------------------
      u_ts = ustar_threshold * H / R

      HorizFlux = MAX(0._fp, (ustar ** 3.0_fp * (1.0_fp - (u_ts / ustar) ** 2.0_fp) * (1.0_fp + (u_ts / ustar) ** 2.0_fp ) ) )

      return
   end subroutine Kawamura_HorizFlux

   !>
   !! \brief Computes the Drag Partition from MB95
   !!
   !! Marticorena, B. and Bergametti, G.: Modeling the atmospheric dust cycle:
   !! 1. Design of a soil-derived dust emission scheme,
   !! J. Geophys. Res.-Atmos., 100, 16415–16430, https://doi.org/10.1029/95JD00690, 1995
   !!
   !! \param z0 roughness length
   !! \param R Drag partition
   !!
   !! \ingroup catchem_dust_process
   !!!>
   subroutine MB95_DragPartition(z0, R)
      IMPLICIT NONE
      ! Parameters
      real(fp), intent(in)  :: z0   !< roughness length [m]
      real(fp), intent(out) :: R    !< Drag partition (0-1)

      ! Local Variables
      real(fp), parameter :: z0s = 0.0008467 !< ideal roughness length of soil

      ! Initialize
      R = ZERO

      !--------------------------------------------
      ! MB95 Drag Partition
      !--------------------------------------------
      R = 1.0_fp - (log(z0 / z0s ) / log(0.7_fp * (0.1_fp / z0s) ** 0.8_fp))
      return

   end subroutine MB95_DragPartition

   !>
   !! \brief Computes the Threshold Friction Velocity from MB97
   !!
   !! Marticorena, B. and Bergametti, G.: Modeling the atmospheric dust cycle:
   !! 1. Design of a soil-derived dust emission scheme,
   !! J. Geophys. Res.-Atmos., 100, 16415–16430, https://doi.org/10.1029/95JD00690, 1995 | TODO fix with correct reference
   !!
   !! \param soil_density soil density
   !! \param air_density air density
   !! \param radius particle radius
   !! \param ustar_threshold threshold friction velocity
   !!
   !! \ingroup catchem_dust_process
   !!!>
   subroutine MB97_threshold_velocity(soil_density, air_density, radius, ustar_threshold)
      ! USES
      USE constants, ONLY: g0
      IMPLICIT NONE

      ! Input Parameters
      !-----------------
      real(fp), intent(in) :: radius       !< particle radius
      real(fp), intent(in) :: soil_density !< soil density
      real(fp), intent(in) :: air_density  !< air density

      ! Output Parameters
      !------------------
      real(fp), intent(out) :: ustar_threshold !< threshold friction velocity


      ! Local Variables
      !-----------------
      real(fp) :: diameter !< diameter of particle [m]

      diameter = 2.0_fp * radius
      ustar_threshold = 0.13_fp * sqrt(soil_density*g0*diameter/air_density) &
         * sqrt(1.0_fp + 6.e-7_fp/(soil_density*g0*diameter**2.5_fp)) &
         / sqrt(1.928_fp*(1331.0_fp*(100._fp*diameter)**1.56_fp+0.38_fp)**0.092_fp - 1.0_fp)

      return

   end subroutine MB97_threshold_velocity


end module CCPr_Dust_Common_Mod
