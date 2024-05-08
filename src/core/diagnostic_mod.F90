!> \file diagnostic_mod.F90
!> \brief Module DIAGNOSTIC\_MOD is used to store diagnostic information
!> \author Barry Baker
!> \date 05/2023
!> \version 0.1

module DiagnosticTypeModule
    implicit none
  
    type DiagnosticType
      
      character(len=:), allocatable :: longName   !< long name for diagnostic
      character(len=:), allocatable :: shortName  !< short name for diagnostic
      character(len=:), allocatable :: units      !< units for diagnostic

      contains

      procedure :: defineByLongName
      procedure :: defineByShortName
      procedure :: defineByUnits
    
    end type DiagnosticType
  
  contains
  
    subroutine defineByLongName(diag, longName, shortName, units)
      class(DiagnosticType), intent(inout) :: diag
      character(len=*), intent(in) :: longName, shortName, units
  
      diag%longName = longName
      diag%shortName = shortName
      diag%units = units
    end subroutine defineByLongName
  
    subroutine defineByShortName(diag, shortName, longName, units)
      class(DiagnosticType), intent(inout) :: this
      character(len=*), intent(in) :: shortName, longName, units
  
      diag%shortName = shortName
      diag%longName = longName
      diag%units = units
    end subroutine defineByShortName
  
    subroutine defineByUnits(diag, units, longName, shortName)
      class(DiagnosticType), intent(inout) :: this
      character(len=*), intent(in) :: units, longName, shortName
  
      diag%units = units
      diag%longName = longName
      diag%shortName = shortName
    end subroutine defineByUnits
  
  end module DiagnosticTypeModule


