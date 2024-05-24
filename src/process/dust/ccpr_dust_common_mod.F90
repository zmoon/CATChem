!>
!! \file ccpr_dust_common_mod.F90
!! \brief Contains module ccpr_dust_common_mod
!!
!! \author Barry Baker
!! \date 05/2024
!!!>
module CCPr_Dust_Common_Mod
    use precision_mod, only: fp, ZERO
    use Error_Mod
    implicit none
    private
    public :: Fecan_SoilMoisture
    public :: Shao_SoilMoisture
    public :: KokDistribution
    public :: Soil_Erosion_Potential
    public :: Draxler_HorizFlux
    public :: DustStateType

    !> \brief Type for CATCHem Dust Process
    !!
    !! \details Contains all the information needed to run the CATCHem Dust Process
    !!
    !! This type contains the following variables:
    !! - Activate : Activate Process (True/False)
    !! - nDustSpecies : Number of dust processes
    !! - SchemeOpt : Scheme Option
    !! - DustSpeciesIndex : Index of dust species
    !! - SpcIDs : CATChem species IDs
    !! - LowerBinRadius : Lower bin radius        [m]
    !! - UpperBinRadius : Upper bin radius        [m]
    !! - EffectiveRadius : Effective radius        [m]
    !! - DustDensity : Dust density            [kg/m^3]
    !! - BetaScaleFactor : Beta Scaling Parameter  [1]
    !! - AlphaScaleFactor : Alpha Scaling Parameter [1]
    !! - TotalEmission : Total emission          [kg/m^2/s]
    !! - EmissionRate : Emission rate            [kg/m^2/s]
    !! - FengshaMoistureOpt : Fengsha-Moisture Calculation Option
    !! - FengshaDragOpt : Fengsha-Drag Calculation Option
    !!!>
    TYPE :: DustStateType
        ! Generic Variables for Every Process
        Logical                         :: Activate            ! Activate Process (True/False)
        INTEGER                         :: nDustSpecies        ! Number of dust processes
        INTEGER                         :: SchemeOpt           ! Scheme Option
        INTEGER, ALLOCATABLE            :: DustSpeciesIndex(:) ! Index of dust species
        INTEGER, ALLOCATABLE            :: SpcIDs(:)           ! CATChem species IDs

        ! Process Specific Parameters
        REAL(fp), ALLOCATABLE           :: LowerBinRadius(:)         ! Lower bin radius        [m]
        REAL(fp), ALLOCATABLE           :: UpperBinRadius(:)         ! Upper bin radius        [m]
        REAL(fp), ALLOCATABLE           :: EffectiveRadius(:)        ! Effective radius        [m]
        REAL(fp), ALLOCATABLE           :: DustDensity(:)            ! Dust density            [kg/m^3]
        REAL(fp)                        :: BetaScaleFactor           ! Beta Scaling Parameter  [1]
        REAL(fp)                        :: AlphaScaleFactor          ! Alpha Scaling Parameter [1]
        REAL(fp), ALLOCATABLE           :: TotalEmission             ! Total emission          [kg/m^2/s]
        REAL(fp), ALLOCATABLE           :: EmissionPerSpecies(:)     ! Emission per species    [kg/m^2/s]

        ! Scheme Options
        INTEGER                         :: FengshaMoistureOpt  ! Fengsha-Moisture Calculation Option
        INTEGER                         :: FengshaDragOpt      ! Fengsha-Drag Calculation Option

     !=================================================================
     ! Module specific variables/arrays/data pointers come below
     !=================================================================

    END TYPE DustStateType

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
    !!!>
    subroutine Fecan_SoilMoisture( clay, sand, volumetric_soil_moisture, H)
        IMPLICIT NONE
        ! Parameters
        real(fp), intent(in)  :: clay                      ! Fractional Clay Content
        real(fp), intent(in)  :: sand                      ! Fractional Sand Content
        real(fp), intent(in)  :: volumetric_soil_moisture  ! Volumetric soil moisture [m3 m-3]
        real(fp), intent(out) :: H                         ! Soil Moisture attenuation factor for dust emission [1]

        ! Local Variables
        real(fp) :: vsat                      ! Saturated volumetric water content (sand-dependent) ! [m3 m-3]
        real(fp) :: gravimetric_soil_moisture ! Gravimetric soil moisture [kg/kg]
        real(fp) :: DryLimit                  ! Dry limit of the soil moisture [kg/kg]

        ! Initialize
        H = ZERO

        !--------------------------------------------
        ! Compute Saturated Volumetric Water Content
        !--------------------------------------------
        vsat = 0.489 - 0.00126 * (100. * sand)

        !--------------------------------------------
        ! Compute Gravimetric Soil moisture
        !--------------------------------------------
        gravimetric_soil_moisture = volumetric_soil_moisture * 1000.0 / (1.0 - vsat)

        !--------------------------------------------
        ! Compute Dry Limit
        !--------------------------------------------
        DryLimit = clay * (14.0 * clay + 17.0)

        !--------------------------------------------
        ! Compute attenuation factor
        !--------------------------------------------
        H = sqrt(1 + 1.21 * max(0., gravimetric_soil_moisture - DryLimit)**0.68)
        return

    end subroutine Fecan_SoilMoisture

    !>
    !! \brief Computes the soil moisture attenuation factor for dust emission
    !!
    !! Zhao, T. L., S. L. Gong, X. Y. Zhang, A. Abdel-Mawgoud, and Y. P. Shao (2006),
    !! An assessment of dust emission schemes in modeling east Asian dust storms,
    !! J. Geophys. Res., 111, D05S90, doi:10.1029/2004JD005746.
    !!
    !! \param volumetric_soil_moisture Volumetric soil moisture
    !! \param H Soil moisture attenuation factor for dust emission
    !!
    !!!>
    subroutine Shao_SoilMoisture( volumetric_soil_moisture, H)
        IMPLICIT NONE
        ! Parameters
        real(fp), intent(in)  :: volumetric_soil_moisture  ! Volumetric soil moisture [m3 m-3]
        real(fp), intent(out) :: H                         ! Soil Moisture attenuation factor for dust emission [1]

        ! Initialize
        H = ZERO

        !--------------------------------------------
        ! Compute attenuation factor
        !--------------------------------------------
        if (volumetric_soil_moisture <= 0.03) THEN
            H = exp(22.7 * volumetric_soil_moisture)
        else
            H = exp(93.5 * volumetric_soil_moisture - 2.029)
        endif

        return

    end subroutine Shao_SoilMoisture

    !>
    !! \brief KokDistribution
    !!
    !! Kok, J. F. (2011a), A scaling theory for the size distribution of emitted
    !! dust aerosols suggests climate models underestimate the size of the global
    !! dust cycle, Proc. Natl. Acad. Sci. U. S. A., 108(3), 1016–1021,
    !! doi:10.1073/pnas.1014798108.
    !!
    !! \param radius Radius
    !! \param rLow Lower radius
    !! \param rUp Upper radius
    !! \param distribution Distribution
    !!!>
    subroutine KokDistribution(radius, rLow, rUp, distribution)
        use constants, only: pi
        IMPLICIT NONE
        ! Parameters
        real(fp), dimension(:), intent(in)  :: radius
        real(fp), dimension(:), intent(in)  :: rLow
        real(fp), dimension(:), intent(in)  :: rUp
        real(fp), dimension(:), intent(out) :: distribution

        ! Local Variables
        integer :: n          ! looping variable
        integer :: nbins      ! number of bins
        real(fp) :: diameter  ! effective diameter of particle
        real(fp) :: dlam      ! diameter / lambda
        real(fp) :: dvol      ! volume of particle

        ! Constants
        real(fp), parameter :: mmd = 3.4                               ! median mass diameter [microns]
        real(fp), parameter :: stddev = 3.0                            ! standard deviation [microns]
        real(fp), parameter :: lambda = 12.0                           ! crack propagation length [um]
        real(fp), parameter :: factor = 1. / ( sqrt(2.) * log(stddev)) ! auxilary constant for the distribution

        ! Initialize
        distribution = ZERO
        nbins = size(radius)

        do n = 1, nbins
            diameter = radius(n) * 2.0
            dlam = diameter / lambda
            dvol = 4. / 3. * pi * diameter**3
            diameter = (1. + erf(factor * log(diameter/mmd))) * exp(-dlam * dlam * dlam) * log(rUp(n)/rLow(n))
            dvol = dvol + distribution(n)
        end do

        ! Normalize Distribution
        do n = 1, nbins
            distribution(n) = distribution(n) / dvol
        end do
        return
    end subroutine KokDistribution

    !>
    !! \brief Computes the soil erosion potential
    !!
    !! TODO: Find reference for this
    !!
    !! \param clayfrac clay fraction
    !! \param sandfrac sand fraction
    !! \param SEP soil erosion potential
    subroutine Soil_Erosion_Potential(clayfrac, sandfrac, SEP)

        IMPLICIT NONE
        ! Parameters
        real(fp), intent(in)  :: clayfrac
        real(fp), intent(in)  :: sandfrac
        real(fp), intent(out) :: SEP

        ! Initialize
        SEP = ZERO

        !--------------------------------------------
        ! Compute SEP
        !--------------------------------------------
        if (clayfrac > 0.0 .and. sandfrac > 0.0) then
           SEP = (0.08 * clayfrac + 0.12 * sandfrac + (1 - sandfrac - clayfrac))
        endif
        return
    end subroutine Soil_Erosion_Potential

    !>
    !! \brief Computes Draxler Hoirizontal Flux
    !!
    !! Draxler, R.R, D.A. Gillette, J.S. Kirkpatrick, and J. Heller (2001),
    !! Estimating PM10 air concentrations from dust storms in Iraq, Kuwait,
    !! and Saudi Arabia, Atm. Environ, 35: 4315-4330.
    !! https://doi.org/10.1016/S1352-2310(01)00159-5
    !!
    !! \param ustar friction velocity
    !! \param ustar_threshold dry threshold fricition velocity
    !! \param R Drag partition
    !! \param H Soil Moisture Attenuation Factor
    !! \param HorizFlux Horizontal Mass Flux
    !!
    !!!>
    subroutine Draxler_HorizFlux(ustar, ustar_threshold, R, H, HorizFlux)
        IMPLICIT NONE
        ! Parameters
        real(fp), intent(in)  :: ustar
        real(fp), intent(in)  :: ustar_threshold
        real(fp), intent(in)  :: R
        real(fp), intent(in)  :: H
        real(fp), intent(out) :: HorizFlux

        ! Local Variables
        ! real(fp) :: ustar_s ! surface Friction Velocity (ie USTAR * R) [m/s]
        real(fp) :: u_ts    ! Modified threshold fricition velocity

        ! Initialize
        HorizFlux = ZERO

        !--------------------------------------------
        ! Compute Draxler Horizontal Flux
        !--------------------------------------------
        u_ts = ustar_threshold * H / R

        if (ustar >= ustar_threshold) then
           HorizFlux = (ustar * R) ** 3.0 * (1 - ( u_ts / ustar ) ** 2.0)
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
    !! \param ustar_threshold dry threshold fricition velocity
    !! \param R Drag partition
    !! \param H Soil Moisture Attenuation Factor
    !! \param HorizFlux Horizontal Mass Flux
    !!
    !!!>
    subroutine Kawamura_HorizFlux(ustar, ustar_threshold, R, H, HorizFlux)
        IMPLICIT NONE
        ! Parameters
        real(fp), intent(in)  :: ustar
        real(fp), intent(in)  :: ustar_threshold
        real(fp), intent(in)  :: R
        real(fp), intent(in)  :: H
        real(fp), intent(out) :: HorizFlux

        ! Local Variables
        real(fp) :: u_ts ! Modified threshold fricition velocity

        ! Initialize
        HorizFlux = ZERO

        !--------------------------------------------
        ! Compute Kawamura Horizontal Flux
        !--------------------------------------------
        u_ts = ustar_threshold * H / R

        HorizFlux = ustar ** 3.0 * (1 - (u_ts / ustar) ** 2.) * (1 + (u_ts / ustar) ** 2.)

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
    !!!>
    subroutine MB95_DragParitition(z0, R)
        
        IMPLICIT NONE
        ! Parameters
        real(fp), intent(in)  :: z0
        real(fp), intent(out) :: R

        ! Local Variables
        real(fp), parameter :: z0s = 0.0008467 ! ideal roughness length of soil

        ! Initialize
        R = ZERO

        !--------------------------------------------
        ! MB95 Drag Paritition
        !--------------------------------------------
        R = 1 - (log(z0 / z0s ) / log(0.7 * (10.0 / z0s) ** 0.8))
        return

    end subroutine MB95_DragParitition

end module CCPr_Dust_Common_Mod
