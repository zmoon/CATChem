!>
!! \file
!! \brief CCPr Scheme for dry deposition
!!
!!
!! Reference: Benchmarking GOCART-2G in the Goddard Earth Observing System (GEOS)
!! Allison B. Collow, Peter R. Colarco, Arlindo M. da Silva, Virginie Buchard,
!! Huisheng Bian, M Chin, Sampa Das, Ravi Govindaraju, Dongchul Kim, and Valentina Aquila,
!! Geosci. Model Development, 17, 14431468, 2024
!! https://doi.org/10.5194/gmd-17-1443-2024
!!
!! \author Lacey Holland
!! \date 07/2024
!!!>
module CCPr_Scheme_GOCART_DryDep_Mod

   implicit none

   private

   public :: CCPr_Scheme_GOCART_DryDep

contains

   !> \brief Brief description of the subroutine
   !!
   !! \param MetState     Meteorological Variables
   !! \param DiagState    Diagnostic Variables
   !! \param DryDepState  DryDeposition Variables
   !! \param RC           Success or Failure
   !!
   !! Note that other state types may be required, e.g. one specific to the process group.
   !!!>

   subroutine CCPr_Scheme_GOCART_DryDep(km,              &
      tmpu,            &
      rhoa,            &
      hghte,           &
      lwi,             &
      ustar,           &
      pblh,            &
      hflux,           &
      von_karman,      &
      cp,              &
      g0,              &
      z0h,             &
      drydepf,         &
      ResuspensionOpt, &
      radius,          &
      rhop,            &
      u10m,             &
      v10m,             &
      fraclake,        &
      gwettop,         &
      RC)

      ! Uses
      USE GOCART2G_Process, only: DryDeposition

      IMPLICIT NONE

      ! Arguments
      INTEGER, intent(in)                     :: km            ! number of vertical levels
      real, intent(in)            :: lwi                       ! orography flag; Land, ocean, ice mask
      REAL, allocatable, intent(in), DIMENSION(:) :: tmpu   ! Temperature [K]
      REAL, allocatable, intent(in), DIMENSION(:) :: rhoa   ! Air density [kg/m^3]
      REAL, allocatable, intent(in), DIMENSION(:) :: hghte  ! Height [m]
      REAL,  intent(in)               :: radius                                ! particle radius [m]
      REAL,  intent(in)               :: rhop                                  ! particle density [kg/m^3]
      REAL,  intent(in)               :: ustar                                 ! friction speed [m/sec]
      REAL,  intent(in)               :: pblh                                  ! PBL height [m]
      REAL,  intent(in)               :: hflux                                 ! sfc. sens. heat flux [W m-2]
      REAL,  intent(in)               :: z0h                                   ! rough height, sens. heat [m]
      REAL,  intent(in)               :: u10m                   ! 10-m u-wind component [m/sec]
      REAL,  intent(in)               :: v10m                   ! 10-m v-wind component [m/sec]
      REAL,  intent(in)               :: fraclake               ! fraction covered by water [1]
      REAL,  intent(in)               :: gwettop                ! fraction soil moisture [1]
      real, intent(in)                        :: cp
      REAL, intent(in)                        :: g0
      real, intent(in)                        :: von_karman
      logical, intent(in)                     ::  ResuspensionOpt
      ! Output
      REAL, intent(out)  :: drydepf
      integer, intent(out) :: RC                      ! Success or Failure

      ! Local Variables
      real, pointer :: GOCART_tmpu(:,:,:)
      real, pointer :: GOCART_rhoa(:,:,:)
      real, pointer :: GOCART_HGHTE(:,:,:)
      real, pointer :: GOCART_LWI(:,:)
      real ,pointer :: GOCART_USTAR(:,:)
      real ,pointer :: GOCART_PBLH(:,:)
      real ,pointer :: GOCART_HFLUX(:,:)
      real ,pointer :: GOCART_Z0H(:,:)
      real ,pointer :: GOCART_U10(:,:)
      real ,pointer :: GOCART_V10(:,:)
      real ,pointer :: GOCART_FRACLAKE(:,:)
      real ,pointer :: GOCART_GWETTOP(:,:)
      real ,pointer :: GOCART_DRYDEPF(:,:)
      character(len=256) :: errMsg
      character(len=256) :: thisLoc

      ! Initialize
      errMsg = ''
      thisLoc = ' -> at CCPr_Scheme_GOCART_DryDep (in CCPr_Scheme_GOCART_mod.F90)'
      RC = 0

      ! transform data for GOCART DryDeposition call

      call PrepMetVarsForGOCART(km,              &
         tmpu,            &
         rhoa,            &
         hghte,           &
         u10m,             &
         v10m,             &
         fraclake,        &
         gwettop,         &
         lwi,             &
         ustar,           &
         pblh,            &
         hflux,           &
         z0h,             &
         GOCART_tmpu,     &
         GOCART_RHOA,     &
         GOCART_HGHTE,    &
         GOCART_U10,      &
         GOCART_V10,      &
         GOCART_FRACLAKE, &
         GOCART_GWETTOP,  &
         GOCART_LWI,      &
         GOCART_USTAR,    &
         GOCART_PBLH,     &
         GOCART_HFLUX,    &
         GOCART_Z0H)

      !------------------
      ! Begin Scheme Code
      !------------------

      if (ResuspensionOpt) then
         call DryDeposition(km, GOCART_TMPU, GOCART_RHOA, GOCART_HGHTE, GOCART_LWI, GOCART_USTAR, &
            GOCART_PBLH, GOCART_HFLUX, von_karman, cp, g0, GOCART_Z0H, GOCART_DRYDEPF, RC, &
            radius, rhop, GOCART_U10, GOCART_V10, GOCART_FRACLAKE, GOCART_GWETTOP)
      else
         call DryDeposition(km, GOCART_TMPU, GOCART_RHOA, GOCART_HGHTE, GOCART_LWI, GOCART_USTAR, &
            GOCART_PBLH, GOCART_HFLUX, von_karman, cp, g0, GOCART_Z0H, GOCART_DRYDEPF, RC)
      endif


      drydepf = GOCART_DRYDEPF(1,1)

      ! End GOCART Code


   end subroutine CCPr_Scheme_GOCART_DryDep

   !>
   !! \brief PrepMetVarsForGOCART - Prep the meteorological variables for GOCART DryDeposition scheme
   !!
   !! \param [INOUT] metstate
   !! \param [INOUT] tmpu
   !! \param [INOUT] rhoa
   !! \param [INOUT] hghte
   !! \param [INOUT] oro
   !! \param [INOUT] ustar
   !! \param [INOUT] pblh
   !! \param [INOUT] shflux
   !! \param [INOUT] z0h
   !! \param [INOUT] u10m
   !! \param [INOUT] v10m
   !! \param [INOUT] fraclake
   !! \param [INOUT] gwettop
   !! \param [OUT] rc
   !!
   !! \ingroup core_modules
   !!!>
   subroutine PrepMetVarsForGOCART(km,              &
      tmpu,            &
      rhoa,            &
      hghte,           &
      u10m,             &
      v10m,             &
      fraclake,        &
      gwettop,         &
      lwi,             &
      ustar,           &
      pblh,            &
      hflux,           &
      z0h,             &
      GOCART_tmpu,     &
      GOCART_RHOA,     &
      GOCART_HGHTE,    &
      GOCART_U10,      &
      GOCART_V10,      &
      GOCART_FRACLAKE, &
      GOCART_GWETTOP,  &
      GOCART_LWI,      &
      GOCART_USTAR,    &
      GOCART_PBLH,     &
      GOCART_HFLUX,    &
      GOCART_Z0H)



      IMPLICIT NONE

      ! INPUTS
      INTEGER, intent(in)                     :: km     ! number of vertical levels
      real,  intent(in)                      :: lwi                                    ! orography flag; Land, ocean, ice mask
      REAL,  intent(in), DIMENSION(:) :: tmpu   ! Temperature [K]
      REAL,  intent(in), DIMENSION(:) :: rhoa   ! Air density [kg/m^3]
      REAL,  intent(in), DIMENSION(:) :: hghte  ! Height [m]
      REAL,  intent(in)                      :: ustar                                 ! friction speed [m/sec]
      REAL,  intent(in)                      :: pblh                                  ! PBL height [m]
      REAL,  intent(in)                      :: hflux                                 ! sfc. sens. heat flux [W m-2]
      REAL,  intent(in)                      :: z0h                                   ! rough height, sens. heat [m]
      REAL,  intent(in) :: u10m                   ! 10-m u-wind component [m/sec]
      REAL,  intent(in) :: v10m                   ! 10-m v-wind component [m/sec]
      REAL,  intent(in) :: fraclake               ! fraction covered by water [1]
      REAL,  intent(in) :: gwettop                ! fraction soil moisture [1]

      ! INPUT/OUTPUTS
      REAL, intent(inout), pointer :: GOCART_TMPU(:,:,:)   !< temperature [K]
      REAL, intent(inout), pointer, DIMENSION(:,:,:) :: GOCART_RHOA   !< air density [kg/m^3]
      REAL, intent(inout), pointer, DIMENSION(:,:,:) :: GOCART_HGHTE  !< geometric height [m]
      REAL, intent(inout) :: GOCART_U10(:,:)                  !< 10-m u-wind component [m/sec]
      REAL, intent(inout) :: GOCART_V10 (:,:)                  !< 10-m v-wind component [m/sec]
      REAL, intent(inout) :: GOCART_FRACLAKE(:,:)               !< fraction covered by water [1]
      REAL, intent(inout) :: GOCART_GWETTOP(:,:)                !< fraction soil moisture [1]
      real,  intent(inout) :: GOCART_LWI(:,:)                                 !< orography flag; Land, ocean, ice mask
      REAL,     intent(inout) :: GOCART_USTAR(:,:)                               !< friction speed [m/sec]
      REAL,     intent(inout) :: GOCART_PBLH(:,:)                               !< PBL height [m]
      REAL,     intent(inout) :: GOCART_HFLUX(:,:)                               !< sfc. sens. heat flux [W m-2]
      REAL,     intent(inout) :: GOCART_Z0H(:,:)                                 !< rough height, sens. heat [m]

      ! OUTPUTS
      INTEGER :: rc !< Return code

      ! Error handling
      character(len=255) :: errMsg
      character(len=255) :: thisloc

      ! if (.not. allocated(GOCART_tmpu)) then
      !    allocate(GOCART_tmpu(1,1, km), stat=rc)
      !    if (RC /= 0) then
      !       print*, 'Could not allocate tmpu in PrepMetVarsForGOCART'
      !    endif
      ! endif

      ! if (.not. allocated(GOCART_RHOA)) then
      !    allocate(GOCART_RHOA(1,1,km), stat=rc)
      !    if (RC /= 0) then
      !       print*, 'Could not allocate rhoa in PrepMetVarsForGOCART'
      !    endif
      ! endif

      ! if (.not. allocated(GOCART_HGHTE)) then
      !    allocate(GOCART_HGHTE(1,1,km), stat=rc)
      !    if (RC /= 0) then
      !       print*, 'Could not allocate hghte in PrepMetVarsForGOCART'
      !    endif
      ! endif

      GOCART_tmpu(1,1,1:km) = tmpu(1:km)         ! temperature [K]
      GOCART_RHOA(1,1,1:km) = rhoa(1:km) ! air density [kg/m^3]
      GOCART_HGHTE(1,1,1:km) = hghte(1:km)    ! top of layer geopotential height [m]
      GOCART_LWI(1,1) = LWI       ! orography flag; Land, ocean, ice mask
      GOCART_USTAR(1,1)  = ustar     ! friction speed [m/sec]
      GOCART_PBLH(1,1)   = pblh      ! PBL height [m]
      GOCART_HFLUX(1,1) = hflux     ! sfc. sens. heat flux [W m-2]
      GOCART_Z0H(1,1)    = z0h       ! rough height, sens. heat [m]
      GOCART_U10(1,1) = u10m
      GOCART_V10(1,1) = v10m
      GOCART_FRACLAKE(1,1) = fraclake
      GOCART_GWETTOP(1,1) = gwettop

   end subroutine PrepMetVarsForGOCART

end module CCPr_Scheme_GOCART_DryDep_Mod
