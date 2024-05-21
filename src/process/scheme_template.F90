!>
!! \file scheme_template.F90
!! \brief This file contains a template for an example scheme for CATChem
!! \author Barry Baker
!! \date 05/2023
!! \version 0.1
!!
!! 
!!

!==============================================================================
! Template for an example scheme for CATChem
!==============================================================================

module scheme_template(input1, input2, inout1, inout2, output1, output2, RC)
    use precision_mod
    use constants
    use error_mod

    implicit none

    !=========================================================================
    ! Parameters 
    !=========================================================================
    !
    ! Input parameters
    !
    ! Integers
    integer, intent(in) :: input1  ! Comment for input1 (units if needed) 

    ! Reals
    real(fp), intent(in) :: input2 ! Comment for input2 (units if needed) 

    ! Booleans

    !
    ! INOUT PARAMETERS
    !
    ! Integers
    integer, intent(inout) :: RC    ! Comment for inout1 (units if needed)

    ! Reals
    real(fp), intent(inout) :: inout2   ! Comment for inout2 (units if needed)

    ! Booleans
    bool, intent(inout) :: inout1  ! Comment for inout1 (units if needed

    !
    ! OUTPUT PARAMETERS
    !
    ! Integers
    integer, intent(out) :: output1  ! Comment for output1 (units if needed)

    ! Reals
    real(fp), intent(out) :: output2 ! Comment for output2 (units if needed)    

    ! Booleans
    ! add boolean parameters here

    !=========================================================================
    ! Local Variables
    !=========================================================================
    
    ! Reals
    real(fp) :: var1
    real(fp) :: var2

    ! Integers
    integer :: var3

    ! Parameters
    parameter :: var4 = 0.0_fp

    !=========================================================================
    ! Error Handling Variables
    !=========================================================================

    thisLoc = 'scheme_template::scheme_template() -> at scheme_template.F90'
    RC = CC_SUCCESS

    !=========================================================================
    ! Begin Code
    !=========================================================================  

    ! add code here
    
end module scheme_template