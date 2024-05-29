!>
!! \file constants.F90
!! \brief This file contains constants for catchem
!!
!! This file contains constants for catchem
module constants

   USE precision_mod

   implicit none

   public

   REAL(fp), PARAMETER :: Cp = 1.0046e+3_fp                !< Specific heat of dry air at constant pressure [J/kg/K]
   REAL(fp), PARAMETER :: Cv = 7.1760e+2_fp                !< Specific heat of dry air at constant volume [J/kg/K]
   REAL(fp), PARAMETER :: AIRMW = 28.9644_fp               !< Average molecular weight of dry air [g/mol]
   REAL(fp), PARAMETER :: H2OMW = 18.016_fp                !< Molecular weight of water [g/mol]
   REAL(fp), PARAMETER :: AVO = 6.022140857e+23_fp         !< Avogadro's number [particles/mol]
   REAL(fp), PARAMETER :: g0     = 9.80665e+0_fp           !<  Acceleration due to gravity at earth's surface [m/s^2]
   REAL(fp), PARAMETER :: g0_100 = 100.0_fp / g0           !<  100 over gravity at earth's surface
   REAL(fp), PARAMETER :: PI     = 3.14159265358979323_fp  !<  Pi
   REAL(fp), PARAMETER :: PI_180 = PI / 180.0_fp           !<  Radians per degree
   REAL(fp), PARAMETER :: Re = 6.3710072e+6_fp             !<  Radius of Earth [m]
   REAL(fp), PARAMETER :: Rd   = 287.0_fp                  !<  Gas Constant in Dry Air [J/K/kg]
   REAL(fp), PARAMETER :: Rdg0 = Rd / g0                   !<  Gas Constant in Dry Air divided by gravity
   REAL(fp), PARAMETER :: Rv = 461.00_fp                   !<  Gas Constant for water vapor [J/K/kg]
   REAL(fp), PARAMETER :: SCALE_HEIGHT = 7600.0_fp         !<  Scale height of atmosphere [m]
   REAL(fp), PARAMETER :: VON_KARMAN = 0.41_fp             !<  Von Karman's constant [-]
   REAL(fp), PARAMETER :: RSTARG = 8.3144598_fp            !<  Molar gas constant [J/K/mol]  (Source: NIST, 2014)
   REAL(fp), PARAMETER :: XNUMOLAIR = AVO / ( AIRMW * 1.e-3_fp )  !<  Molecules dry air per kg dry air
   REAL(fp), PARAMETER :: BOLTZ = 1.38064852e-23_fp        !<  Boltzmann's constant [J/K] (Source: NIST, 2014)
   REAL(fp), PARAMETER :: ATM = 1.01325e+5_fp              !<  Standard atmosphere [Pa] (Source: NIST, 2014)
   REAL(fp), PARAMETER :: PLANCK = 6.62606957e-34_fp       !<  Plank's constant
   REAL(fp), PARAMETER :: CCONST = 2.99792458e+8_fp        !<  Speed of light [m/s]
   REAL(fp), PARAMETER :: CONSVAP = 6.1078e+03_fp / ( BOLTZ * 1e+7_fp ) !<  Condensation vapor pressure
   REAL(fp), PARAMETER :: RGASLATM = 8.2057e-2_fp          !<  Gas constant [L.atm/K.mole]
   REAL(fp), PARAMETER :: MWCARB = 12.01e-3_fp             !<  Molecular weight of carbon [g/mol]
   REAL(fp), PARAMETER :: E = 2.718281828459045235360287471352_fp  !< Euler's number

end module constants
