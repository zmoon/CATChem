!>
!! \file
!! \brief Contains a Template for any given CCPr Scheme
!!
!! To use:
!! - replace SCHEME by an identifier for the scheme (e.g. 'fengsha')
!! - replace PROCESS by an identifier for the process group (e.g. 'Dust')
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
   !! \param DustState    Dust Variables
   !! \param RC           Success or Failure
   !!
   !! Note that other state types may be required, e.g. one specific to the process group.
   !!!>
   subroutine CCPr_Scheme_GOCART_DryDep(MetState, DiagState, RC)

      ! Uses
      USE Constants,     Only : g0 ! Example to pull in a constant from the CONSTANTS MODULE < Modify as needed >
      use precision_mod, only : fp, ZERO  ! Example to pull in a precision from the PRECISION MODULE < Modify as needed >
      Use MetState_Mod,  Only : MetStateType  ! Needed to access Meteorological Variables
      Use DiagState_Mod, Only : DiagStateType ! Diagnostic Variables are added through DiagState below
      Use Error_Mod,     Only : CC_SUCCESS    ! Error Check Success
      Use CCPr_DryDep_mod, Only : DryDepStateType  ! Overall DryDep State Type - Controlling DryDep

      IMPLICIT NONE

      ! Arguments
      type(MetStateType),  intent(in) :: MetState     ! Meteorological Variables
      type(DiagStateType), intent(in) :: DiagState    ! Diagnostic Variables
      type(DryDepStateType), intent(in) :: DryDepState  ! DryDep Variables

      integer, intent(out) :: RC                      ! Success or Failure

      ! Local Variables
      character(len=256) :: errMsg
      character(len=256) :: thisLoc

      integer :: GOCARTInt   ! Add Local Scheme Specific Variables < Modify as needed >
      real(fp) :: GOCARTFlt  ! Add Local Scheme Specific Variables < Modify as needed >

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

      ! End GOCART Code

   end subroutine CCPr_Scheme_GOCART_DryDep

end module CCPr_Scheme_GOCART_DryDep_Mod
