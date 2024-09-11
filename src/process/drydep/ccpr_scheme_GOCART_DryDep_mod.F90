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
   subroutine CCPr_Scheme_GOCART_DryDep(km,         &
                                        tmpu,       &
                                        rhoa,       &
                                        hghte,      &
                                        lwi,        &
                                        ustar,      &
                                        pblh,       &
                                        hflux,      &
                                        von_karman, &
                                        cp,         &
                                        g0,         &
                                        radius,     &
                                        rhop,       &
                                        u10,        &
                                        v10,        &
                                        fraclake,   &
                                        gwettop,    &
                                        ResuspensionOpt, &
                                        drydepf, &
                                        RC)

      ! Uses
      USE GOCART2G, only: DryDeposition

      IMPLICIT NONE

      ! Arguments
      INTEGER, intent(in)                     :: km     ! number of vertical levels 
      INTEGER, pointer, intent(in)                      :: lwi                                    ! orography flag; Land, ocean, ice mask
                     ! Deposition frequency [1/sec]
      REAL, pointer, intent(in), DIMENSION(:) :: tmpu   ! Temperature [K]
      REAL, pointer, intent(in), DIMENSION(:) :: rhoa   ! Air density [kg/m^3]
      REAL, pointer, intent(in), DIMENSION(:) :: hghte  ! Height [m]
      REAL, pointer, intent(in)                      :: radius                                ! particle radius [m]
      REAL, pointer, intent(in)                      :: rhop                                  ! particle density [kg/m^3]
      REAL, pointer, intent(in)                      :: ustar                                 ! friction speed [m/sec]
      REAL, pointer, intent(in)                      :: pblh                                  ! PBL height [m]
      REAL, pointer, intent(in)                      :: hflux                                 ! sfc. sens. heat flux [W m-2]
      REAL, pointer, intent(in)                      :: z0h                                   ! rough height, sens. heat [m]
      REAL, pointer, intent(in) :: u10m                   ! 10-m u-wind component [m/sec]
      REAL, pointer, intent(in) :: v10m                   ! 10-m v-wind component [m/sec]
      REAL, pointer, intent(in) :: fraclake               ! fraction covered by water [1]
      REAL, pointer, intent(in) :: gwettop                ! fraction soil moisture [1]
      real, intent(in) :: radius 
      real, intent(in) :: rhop
      real, intent(in) :: cp
      real, intent(in) :: von_karman
      logical, intent(in) ::  ResuspensionOpt
      ! Output
      REAL, intent(out)  :: drydepf
      integer, intent(out) :: RC                      ! Success or Failure

      ! Local Variables
      real, allocatable, dimension(:,:,:) :: GOCART_tmpu 
      real, allocatable, dimension(:,:,:)  :: GOCART_rhoa
      real :: GOCART_LWI
      real, allocatable, dimension(:,:,:) :: GOCART_HGHTE
      real :: GOCART_USTAR
      real :: GOCART_PBLH
      real :: GOCART_HFLUX
      real :: GOCART_Z0H
      real :: GOCART_U10
      real :: GOCART_V10
      real :: GOCART_FRACLAKE
      real :: GOCART_GWETTOP
      real :: GOCART_DRYDEPF
      character(len=256) :: errMsg
      character(len=256) :: thisLoc

      ! Initialize
      errMsg = ''
      thisLoc = ' -> at CCPr_Scheme_GOCART_DryDep (in CCPr_Scheme_GOCART_mod.F90)'
      RC = CC_SUCCESS

      ! transform data for GOCART DryDeposition call 

      call PrepMetVarsForGOCART(km,              &
                                tmpu,            &
                                rhoa,            &
                                hghte,           &
                                u10,             &
                                v10,             &
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

      if (ResuspentionOpt) then 
         call DryDeposition(km, GOCART_TMPU, GOCART_RHOA, GOCART_HGHTE, GOCART_LWI, GOCART_USTAR, & 
                            GOCART_PBLH, von_karman, cp, g0, GOCART_Z0H, GOCART_DRYDEPF, RC, &
                            radius, rhop, GOCART_U10, GOCART_V10, GOCART_FRACLAKE, GOCART_GWETTOP)
      else 
         call DryDeposition(km, GOCART_TMPU, GOCART_RHOA, GOCART_HGHTE, GOCART_LWI, GOCART_USTAR, & 
                            GOCART_PBLH, von_karman, cp, g0, GOCART_Z0H, GOCART_DRYDEPF, RC)
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
                                u10,             &
                                v10,             &
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
                                GOCART_Z0)
   

   IMPLICIT NONE

   ! INPUTS
   INTEGER, intent(in)                     :: km     ! number of vertical levels 
   INTEGER, pointer, intent(in)                      :: lwi                                    ! orography flag; Land, ocean, ice mask
   REAL, pointer, intent(in), DIMENSION(:) :: tmpu   ! Temperature [K]
   REAL, pointer, intent(in), DIMENSION(:) :: rhoa   ! Air density [kg/m^3]
   REAL, pointer, intent(in), DIMENSION(:) :: hghte  ! Height [m]
   REAL, pointer, intent(in)                      :: radius                                ! particle radius [m]
   REAL, pointer, intent(in)                      :: rhop                                  ! particle density [kg/m^3]
   REAL, pointer, intent(in)                      :: ustar                                 ! friction speed [m/sec]
   REAL, pointer, intent(in)                      :: pblh                                  ! PBL height [m]
   REAL, pointer, intent(in)                      :: hflux                                 ! sfc. sens. heat flux [W m-2]
   REAL, pointer, intent(in)                      :: z0h                                   ! rough height, sens. heat [m]
   REAL, pointer, intent(in) :: u10m                   ! 10-m u-wind component [m/sec]
   REAL, pointer, intent(in) :: v10m                   ! 10-m v-wind component [m/sec]
   REAL, pointer, intent(in) :: fraclake               ! fraction covered by water [1]
   REAL, pointer, intent(in) :: gwettop                ! fraction soil moisture [1]

   ! INPUT/OUTPUTS
   REAL, intent(inout), allocatable, DIMENSION(:,:,:) :: GOCART_TMPU   !< temperature [K]
   REAL, intent(inout), allocatable, DIMENSION(:,:,:) :: GOCART_RHOA   !< air density [kg/m^3]
   REAL, intent(inout), allocatable, DIMENSION(:,:,:) :: GOCART_HGHTE  !< geometric height [m]
   REAL, DIMENSION(1,1), intent(inout) :: GOCART_U10                  !< 10-m u-wind component [m/sec]
   REAL, DIMENSION(1,1), intent(inout) :: GOCART_V10                  !< 10-m v-wind component [m/sec]
   REAL, DIMENSION(1,1), intent(inout) :: GOCART_FRACLAKE              !< fraction covered by water [1]
   REAL, DIMENSION(1,1), intent(inout) :: GOCART_GWETTOP               !< fraction soil moisture [1]
   INTEGER, intent(inout) :: GOCART_LWI                                !< orography flag; Land, ocean, ice mask
   REAL,    intent(inout) :: GOCART_USTAR                              !< friction speed [m/sec]
   REAL,    intent(inout) :: GOCART_PBLH                               !< PBL height [m]
   REAL,    intent(inout) :: GOCART_HFLUX                              !< sfc. sens. heat flux [W m-2]
   REAL,    intent(inout) :: GOCART_Z0H                                !< rough height, sens. heat [m]

   ! OUTPUTS
   INTEGER :: rc !< Return code

   ! Error handling
   character(len=255) :: errMsg
   character(len=255) :: thisloc

   ! Initialize
   rc = CC_SUCCESS
   errMsg = ''
   thisloc = ' -> at PrepMetVarsForGOCART (in process/drydep/ccpr_DryDep_mod.F90)'

   if (.not. allocated(GOCART_tmpu)) then
      allocate(GOCART_tmpu(1,1, km), stat=rc)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Could not allocate tmpu'
         CALL CC_Error( errMsg, RC, thisLoc )
      endif
   endif

   if (.not. allocated(GOCART_RHOA)) then
      allocate(GOCART_RHOA(1,1,km), stat=rc)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Could not allocate rhoa'
         CALL CC_Error( errMsg, RC, thisLoc )
      endif
   endif

   if (.not. allocated(GOCART_HGHTE)) then
      allocate(GOCART_HGHTE(1,1,km), stat=rc)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Could not allocate hghte'
         CALL CC_Error( errMsg, RC, thisLoc )
      endif
   endif

   GOCART_tmpu = reshape(tmpu, (/1, 1, km/))         ! temperature [K]
   GOCART_RHOA = reshape(AIRDEN, (/1, 1, km/)) ! air density [kg/m^3]
   GOCART_HGHTE = reshape(hghte, (/1, 1, km/))    ! top of layer geopotential height [m]
   GOCART_LWI    = LWI       ! orography flag; Land, ocean, ice mask
   GOCART_USTAR  = ustar     ! friction speed [m/sec]
   GOCART_PBLH   = pblh      ! PBL height [m]
   GOCART_HFLUX = hflux     ! sfc. sens. heat flux [W m-2]
   GOCART_Z0H    = z0h       ! rough height, sens. heat [m]
   GOCART_U10 = u10
   GOCART_V10 = v10
   GOCART_FRACLAKE = fraclake
   GOCART_GWETTOP = gwettop

end subroutine PrepMetVarsForGOCART

end module CCPr_Scheme_GOCART_DryDep_Mod
