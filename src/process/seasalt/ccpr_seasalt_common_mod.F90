!>
!! \file ccpr_SeaSalt_common_mod.F90
!! \brief Contains module ccpr_SeaSalt_common_mod
!!
!! \author Barry Baker
!! \date 05/2024
!!!>
module CCPr_SeaSalt_Common_Mod
   use precision_mod, Only : fp

   implicit none

   private

   PUBLIC :: SeasaltEmissionGong

   !> \brief Type for CATCHem SeaSalt Process
   !!
   !! \details Contains all the information needed to run the CATChem SeaSalt Process
   !!
   !! This type contains the following variables:
   !! - Activate : Activate Process (True/False)
   !! - nSeaSaltSpecies : Number of SeaSalt processes
   !! - SchemeOpt : Scheme Option
   !! - SeaSaltSpeciesIndex : Index of SeaSalt species
   !! - SpcIDs : CATChem species IDs
   !! - LowerBinRadius : Lower bin radius [m]
   !! - UpperBinRadius : Upper bin radius [m]
   !! - EffectiveRadius : Effective radius [m]
   !! - SeaSaltDensity : SeaSalt density [kg/m^3]
   !! - BetaScaleFactor : Beta Scaling Parameter  [1]
   !! - AlphaScaleFactor : Scaling Parameter [1]
   !! - TotalEmission : Total emission [kg/m^2/s]
   !! - EmissionRate : Emission rate   [kg/m^2/s]
   !!!>
   TYPE, PUBLIC :: SeaSaltStateType
      ! Generic Variables for Every Process
      Logical                         :: Activate               ! Activate Process (True/False)
      INTEGER                         :: nSeaSaltSpecies        ! Number of SeaSalt processes
      INTEGER                         :: SchemeOpt              ! Scheme Option
      INTEGER, ALLOCATABLE            :: SeaSaltSpeciesIndex(:) ! Index of SeaSalt species
      INTEGER, ALLOCATABLE            :: SpcIDs(:)              ! CATChem species IDs

      ! Process Specific Parameters
      REAL(fp), ALLOCATABLE           :: LowerBinRadius(:)      ! Lower bin radius        [m]
      REAL(fp), ALLOCATABLE           :: UpperBinRadius(:)      ! Upper bin radius        [m]
      REAL(fp), ALLOCATABLE           :: EffectiveRadius(:)     ! Effective radius        [m]
      REAL(fp), ALLOCATABLE           :: SeaSaltDensity(:)      ! SeaSalt density         [kg/m^3]
      REAL(fp), ALLOCATABLE           :: TotalEmission          ! Total emission          [kg/m^2/s]
      REAL(fp), ALLOCATABLE           :: TotalNumberEmission    ! Total Number Emitted   [#/m2/s]
      REAL(fp), ALLOCATABLE           :: EmissionPerSpecies(:)  ! Emission per species    [kg/m^2/s]
      REAL(fp), ALLOCATABLE           :: NumberEmissionBin(:)   ! Particle Number emission per species [#/m^2/s]
      REAL(fp), ALLOCATABLE           :: SeaSaltScaleFactor     ! SeaSalt Tuning Parameter [-]

      !=================================================================
      ! Module specific variables/arrays/data pointers come below
      !=================================================================

   END TYPE SeaSaltStateType


contains

   !>
   !! \brief Function to compute sea salt emissions following the Gong style parameterization.
   !!
   !! Functional form is from Gong 2003:
   !!  dN/dr = scalefac * 1.373 * (w^wpow) * (r^-aFac) * (1+0.057*r^rpow) * 10^(exppow*exp(-bFac^2))
   !! where r is the particle radius at 80% RH, dr is the size bin width at 80% RH, and w is the wind speed
   !!!>
   function SeasaltEmissionGong ( r, dr, w, scalefac, aFac, bFac, rpow, exppow, wpow )

      real(fp), intent(in) :: r         !< Wet particle radius [um]
      real(fp), intent(in) :: dr        !< Wet particle bin width [um]
      real(fp), intent(in) :: w         !< Grid box mean wind speed [m s-1] (10-m or ustar wind)
      real(fp), intent(in) :: scalefac  !< scale factor
      real(fp), intent(in) :: aFac
      real(fp), intent(in) :: bFac
      real(fp), intent(in) :: rpow
      real(fp), intent(in) :: exppow
      real(fp), intent(in) :: wpow
      real(fp)             :: SeasaltEmissionGong

      !  Initialize
      SeasaltEmissionGong = 0.

      !  Particle size distribution function
      SeasaltEmissionGong = scalefac * 1.373_fp*r**(-aFac)*(1._fp+0.057_fp*r**rpow) &
         *10._fp**(exppow*exp(-bFac**2._fp))*dr
      !  Apply wind speed function
      SeasaltEmissionGong = w**wpow * SeasaltEmissionGong

   end function SeasaltEmissionGong

end module CCPr_SeaSalt_Common_Mod
