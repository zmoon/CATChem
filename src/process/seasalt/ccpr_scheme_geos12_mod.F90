!>
!! \file
!! \brief Contains the Gong 1997
!!
!! Gong, S. L., L. A. Barrie, and J.-P. Blanchet (1997), Modeling sea-salt aerosols in the atmosphere:
!! 1. Model development, J. Geophys. Res., 102(D3), 3805–3818, doi:10.1029/96JD02953.
!!
!! \author Barry baker
!! \date 05/2024
!!!>
module CCPr_Scheme_GEOS12_Mod

   implicit none

   private

   public :: CCPr_Scheme_Gong97

contains

   !> \brief Run the Ginoux windblown dust emission scheme
   !!
   !! \param [IN] MetState The MetState object
   !! \param [INOUT] DiagState The DiagState object
   !! \param [INOUT] ChemState The ChemState object
   !! \param [INOUT] SeaSalt The SeaSalt object
   !! \param [OUT] RC Return code
   !!!>
   subroutine CCPr_Scheme_GEOS12(MetState, DiagState, SeaSaltState, RC)

      ! Uses
      Use CCPr_SeaSalt_Common_Mod
      use precision_mod, only : fp, ZERO
      use constants,     only : PI
      Use MetState_Mod,  Only : MetStateType
      Use DiagState_Mod, Only : DiagStateType
      Use Error_Mod,     Only : CC_SUCCESS, CC_FAILURE, CC_Error
      Use CCPr_SeaSalt_Common_Mod, Only : SeaSaltStateType

      implicit none

      ! Arguments
      type(MetStateType),     intent(in)    :: MetState   !< Meteorological Variables
      type(DiagStateType),    intent(inout) :: DiagState  !< Diagnostic Variables
      type(SeaSaltStateType), intent(inout) :: SeaSaltState  !< Dust Variables
      integer, intent(out) :: RC

      ! Local Variables
      character(len=256) :: errMsg
      character(len=256) :: thisLoc
      logical :: do_seasalt                            !< Enable Dust Calculation Flag
      integer :: n, ir                                 !< loop couters
      integer :: nbins                                 !< number of SeaSalt bins
      real(fp), allocatable :: EmissionBin(:)          !< Emission Rate per Bin [kg/m2/s]
      real(fp), allocatable :: NumberEmissionBin(:)    !< Number of particles emitted per bin [#/m2/s]
      integer, parameter :: nr = 10                    !< Number of (linear) sub-size bins
      real, parameter    :: r80fac = 1.65              !< ratio of radius(RH=0.8)/radius(RH=0.) [Gerber]
      real(fp) :: DryRadius                            !< sub-bin radius         (dry, um)
      real(fp) :: DeltaDryRadius                       !< sub-bin radius spacing (dry, um)
      real(fp) :: rwet, drwet                          !< sub-bin radius spacing (rh=80%, um)
      real(fp) :: NumberEmissions                      !< sub-bin number emission rate [#/m2/s]
      real(fp) :: MassEmissions                        !< sub-bin number emission rate [kg/m2/s]
      real(fp) :: aFac
      real(fp) :: bFac
      real(fp) :: scalefac
      real(fp) :: rpow
      real(fp) :: exppow
      real(fp) :: wpow
      real(fp) :: MassScaleFac

      ! Initialize
      errMsg = ''
      thisLoc = ' -> at CCPr_Scheme_Gong97 (in ccpr_scheme_gong97_mod.F90)'
      RC = CC_FAILURE

      nbins = size(SeaSaltState%EffectiveRadius)
      ALLOCATE(EmissionBin(nbins), STAT=RC)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Error Allocating EmissionBin'
         call CC_Error(errMsg, RC, thisLoc)
         return
      endif

      ALLOCATE(NumberEmissionBin(nbins), STAT=RC)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Error Allocating EmissionBin'
         call CC_Error(errMsg, RC, thisLoc)
         return
      endif

      SeaSaltState%EmissionPerSpecies = ZERO
      MassEmissions = ZERO
      NumberEmissions = ZERO
      !--------------------------------------------------------------------
      ! Don't do Sea Salt over certain criteria
      !--------------------------------------------------------------------
      do_seasalt = .true. ! Default value for all cases

      ! Don't do Sea Salt over land
      !----------------------------------------------------------------
      if (MetState%IsLand) then
         do_seasalt = .false.
      endif

      if (MetState%IsIce) then
         do_seasalt = .false.
      endif

      if (MetState%IsSnow) then
         do_seasalt = .false.
      endif

      if (do_seasalt) then

         ! Gong 03 Params
         !---------------
         scalefac = 33.0e3_fp
         rpow     = 3.45_fp
         exppow   = 1.607_fp
         wpow     = 3.41_fp - 1._fp

         do n = 1, nbins

            ! delta dry radius
            !-----------------
            DeltaDryRadius = (SeaSaltState%UpperBinRadius(n) - SeaSaltState%LowerBinRadius(n) ) / nr

            ! Dry Radius Substep
            !-------------------
            DryRadius = SeaSaltState%LowerBinRadius(n) + 0.5 * DeltaDryRadius

            ! Mass scale fcator
            MassScaleFac = scalefac * 4._fp/3._fp*PI*MetState%AIRDEN(1)*(DryRadius**3._fp) * 1.e-18_fp

            do ir = 1, nr ! SubSteps

               ! Effective Wet Radius in Sub Step
               rwet  = r80fac * DryRadius

               ! Effective Delta Wet Radius
               drwet = r80fac * DeltaDryRadius

               aFac     = 4.7_fp*(1._fp + 30._fp*rwet)**(-0.017_fp*rwet**(-1.44_fp))
               bFac     = (0.380_fp-log10(rwet))/0.65_fp

               ! Number emissions flux (# m-2 s-1)
               NumberEmissions = NumberEmissions + SeasaltEmissionGong( rwet, drwet, MetState%USTAR, scalefac, aFac, bFac, rpow, exppow, wpow )

               ! Mass emissions flux (kg m-2 s-1)
               MassEmissions = MassEmissions + SeasaltEmissionGong( rwet, drwet, MetState%USTAR, scalefac, aFac, bFac, rpow, exppow, wpow )

               DryRadius = DryRadius + DeltaDryRadius

            enddo

            SeaSaltState%EmissionPerSpecies(n) = MassEmissions
            SeaSaltState%NumberEmissionBin(n) = NumberEmissions

            MassEmissions = ZERO
            NumberEmissions = ZERO

         enddo ! nbins

         SeaSaltState%TotalEmission = sum(SeaSaltState%EmissionPerSpecies)
         DiagState%sea_salt_total_flux = SeaSaltState%TotalEmission
         SeaSaltState%TotalNumberEmission = sum(SeaSaltState%NumberEmissionBin)

      endif ! do_Sea Salt

      RC = CC_SUCCESS
   end subroutine CCPr_Scheme_Gong97

end module CCPr_Scheme_Gong97_Mod
