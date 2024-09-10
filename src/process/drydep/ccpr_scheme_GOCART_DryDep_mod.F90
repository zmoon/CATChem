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
   subroutine CCPr_Scheme_GOCART_DryDep(MetState, DiagState, RC)

      ! Uses
      USE Constants,     Only : g0, von_karman, cp
      use precision_mod, only : fp, ZERO
      Use MetState_Mod,  Only : MetStateType  ! Needed to access Meteorological Variables
      Use DiagState_Mod, Only : DiagStateType ! Diagnostic Variables are added through DiagState below
      Use Error_Mod,     Only : CC_SUCCESS    ! Error Check Success
      Use CCPr_DryDep_mod                     ! Overall DryDep State Type - Controlling DryDep

      IMPLICIT NONE

      ! Arguments
      type(MetStateType),  intent(in) :: MetState     ! Meteorological Variables
      type(DiagStateType), intent(in) :: DiagState    ! Diagnostic Variables
      !type(DryDepStateType), intent(inout) :: DryDepState  ! DryDep Variables

      integer, intent(out) :: RC                      ! Success or Failure

      ! Local Variables
      character(len=256) :: errMsg
      character(len=256) :: thisLoc

      !integer :: processInt   ! Add Local Scheme Specific Variables < Modify as needed >
      !real(fp) :: processFlt  ! Add Local Scheme Specific Variables < Modify as needed >

      ! Initialize
      errMsg = ''
      thisLoc = ' -> at CCPr_Scheme_GOCART_DryDep (in CCPr_Scheme_GOCART_mod.F90)'
      RC = CC_SUCCESS

      !------------------
      ! Begin Scheme Code
      !------------------

      ! Begin GOCART Code

      ! GOCART Options comes in from DryDepState
      ! Diagnostic Variables are added through DiagState below

      ! ADD GOCART CODE HERE
        


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
subroutine PrepMetVarsForGOCART(metstate, tmpu, rhoa, hghte, lwi, ustar, pblh, hflux, &
   z0h, u10m, v10m, fraclake, gwettop, rc)
   use MetState_Mod, only: MetStateType

   IMPLICIT NONE

   ! INPUTS
   type(MetStateType), intent(in) :: MetState                   !< Meteorological state

   ! INPUT/OUTPUTS
   REAL, intent(inout), allocatable, DIMENSION(:,:,:) :: tmpu   !< temperature [K]
   REAL, intent(inout), allocatable, DIMENSION(:,:,:) :: rhoa   !< air density [kg/m^3]
   REAL, intent(inout), allocatable, DIMENSION(:,:,:) :: hghte  !< geometric height [m]
   REAL, DIMENSION(1,1), intent(inout) :: u10m                  !< 10-m u-wind component [m/sec]
   REAL, DIMENSION(1,1), intent(inout) :: v10m                  !< 10-m v-wind component [m/sec]
   REAL, DIMENSION(1,1), intent(inout) :: fraclake              !< fraction covered by water [1]
   REAL, DIMENSION(1,1), intent(inout) :: gwettop               !< fraction soil moisture [1]
   INTEGER, intent(inout) :: lwi                                !< orography flag; Land, ocean, ice mask
   REAL,    intent(inout) :: ustar                              !< friction speed [m/sec]
   REAL,    intent(inout) :: pblh                               !< PBL height [m]
   REAL,    intent(inout) :: hflux                             !< sfc. sens. heat flux [W m-2]
   REAL,    intent(inout) :: z0h                                !< rough height, sens. heat [m]

   ! OUTPUTS
   INTEGER :: rc !< Return code

   ! LOCAL VARIABLES
   INTEGER :: km

   ! Error handling
   character(len=255) :: errMsg
   character(len=255) :: thisloc

   ! Initialize
   rc = CC_SUCCESS
   errMsg = ''
   thisloc = ' -> at PrepMetVarsForGOCART (in process/drydep/ccpr_DryDep_mod.F90)'

   km  = MetState%NLEVS

   if (.not. allocated(tmpu)) then
      allocate(tmpu(1,1, km), stat=rc)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Could not allocate tmpu'
         CALL CC_Error( errMsg, RC, thisLoc )
      endif
   endif

   if (.not. allocated(rhoa)) then
      allocate(rhoa(1,1,km), stat=rc)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Could not allocate rhoa'
         CALL CC_Error( errMsg, RC, thisLoc )
      endif
   endif

   if (.not. allocated(hghte)) then
      allocate(hghte(1,1,km), stat=rc)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Could not allocate hghte'
         CALL CC_Error( errMsg, RC, thisLoc )
      endif
   endif

   tmpu = reshape(metstate%T, (/1, 1, km/))         ! temperature [K]
   rhoa = reshape(metstate%AIRDEN, (/1, 1, km/)) ! air density [kg/m^3]
   hghte = reshape(metstate%ZMID, (/1, 1, km/))    ! top of layer geopotential height [m]
   lwi    = metstate%LWI       ! orography flag; Land, ocean, ice mask
   ustar  = metstate%ustar     ! friction speed [m/sec]
   pblh   = metstate%pblh      ! PBL height [m]
   hflux = metstate%hflux     ! sfc. sens. heat flux [W m-2]
   z0h    = metstate%z0h       ! rough height, sens. heat [m]

end subroutine PrepMetVarsForGOCART

end module CCPr_Scheme_GOCART_DryDep_Mod
