!>
!! \file
!! \brief Contains a Template for any given CCPr Scheme
!!
!! Reference here: Please include citation here
!!
!! \author Barry baker
!! \date 05/2024
!!!>
module CCPr_Scheme_<SCHEME>_Mod

implicit none

private

public :: CCPr_Scheme_<SCHEME>

contains

 !> \brief Brief description of the subroutine
 !!
 !! \param MetState     Meteorological Variables
 !! \param DiagState    Diagnostic Variables
 !! \param DustState    Dust Variables
 !! \param RC           Success or Failure
 !!!>
subroutine CCPr_Scheme_<SCHEME>(MetState, DiagState, DustState, RC)

   ! Uses
   USE Constants,     Only : g0 ! Example to pull in a constant from the CONSTANTS MODULE < Modify as needed >
   use precision_mod, only : fp, ZERO  ! Example to pull in a precision from the PRECISION MODULE < Modify as needed >
   Use MetState_Mod,  Only : MetStateType  ! Needed to access Meteorological Variables
   Use DiagState_Mod, Only : DiagStateType ! Diagnostic Variables are added through DiagState below
   Use Error_Mod,     Only : CC_SUCCESS    ! Error Check Success
   Use CCPr_<PROCESS>_mod, Only : <PROCESS>StateType  ! Overall PROCESS State Type - Controlling PROCESS

   IMPLICIT NONE

   ! Arguments
   type(MetStateType),  intent(in) :: MetState     ! Meteorological Variables
   type(DiagStateType), intent(in) :: DiagState    ! Diagnostic Variables
   type(<PROCESS>StateType), intent(in) :: <PROCESS>State    ! <PROCESS> Variables

   integer, intent(out) :: RC                      ! Success or Failure

   ! Local Variables
   character(len=256) :: errMsg
   character(len=256) :: thisLoc

   integer :: <SCHEME>Int   ! Add Local Scheme Specific Variables < Modify as needed >
   real(fp) :: <SCHEME>Flt  ! Add Local Scheme Specific Variables < Modify as needed >

   ! Initialize
   errMsg = ''
   thisLoc = ' -> at CCPr_Scheme_<SCHEME> (in CCPr_Scheme_<SCHEME>_mod.F90)'
   RC = CC_SUCCESS


   !------------------
   ! Begin Scheme Code
   !------------------

   ! Begin <SCHEME> Code

   ! <Scheme> Options comes in from <Process>State
   ! Diagnostic Variables are added through DiagState below

   ! End <SCHEME> Code



   end subroutine CCPr_Scheme_<SCHEME>

   end module CCPr_Scheme_<SCHEME>_Mod
