!>
!! \file constants.F90
!! \brief This file contains constants for catchem
!! \author Barry Baker
!! \date 05/2023
!! \version 0.1
!!
!! This file contains constants for catchem 
module constants

  USE precision_mod
  
  implicit none

  public

  real(kind=fp),parameter:: grav_con = 9.80665e+0         !< gravity (\f$m/s^{2}\f$)
  real(kind=fp),parameter:: rd_con   = 2.8705e+2          !< gas constant air (\f$J/kg/K\f$)
  real(kind=fp),parameter:: h2o_con  = 4.6150e+2          !< gas constant H2O (\f$J/kg/K\f$)
  real(kind=fp),parameter:: cp_con   = 1.0046e+3          !< spec heat air at p (\f$J/kg/K\f$)
  real(kind=fp),parameter:: cv_con   = 7.1760e+2          !< spec heat air at v (\f$J/kg/K\f$)
  real(kind=fp),parameter:: pi_con   = 4.0d0*atan(1.0d0)  !< pi

end module constants
