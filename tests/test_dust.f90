program test_dust
   use CATChem, fp => cc_rk
   use testing_mod, only: assert
   implicit none

   type(ConfigType) :: Config
   type(ChemStateType) :: ChemState
   type(MetStateType) :: MetState
   type(DiagStateType) :: DiagState
   type(DustStateType) :: DustState

   character(len=:), allocatable :: title

   ! Error Handling variables
   integer :: rc
   CHARACTER(LEN=255) :: ErrMsg, thisLoc

   ! Error handling
   rc = CC_SUCCESS
   ErrMsg = ''
   thisLoc = ' -> at main (in tests/test_dust.f90)'

   write(*,*) '   CCCCC      A     TTTTTTT   CCCCC  H'
   write(*,*) '  C          A A       T     C       H       CCCC    EEEE  M       M'
   write(*,*) '  C         AAAAA      T     C       HHHHH  C      E    E  M M   M M'
   write(*,*) '  C        A     A     T     C       H   H  C      E EE    M   M   M'
   write(*,*) '   CCCCC  A       A    T      CCCCC  H   H   CCCC   EEEEE  M       M'
   write(*,*) ''
   write(*,*) ''

   ! Configuration Options
   Config%dust_activate = .true.
   Config%dust_scheme = 1
   Config%dust_drag_opt = 1
   Config%dust_moist_opt = 1
   Config%dust_horizflux_opt = 1

   ! Set number of dust species to zero for now
   ChemState%nSpeciesDust = 0

   ! Meteorological State
   MetState%DSOILTYPE = 1
   MetState%SSM = 0.5_fp
   MetState%RDRAG = 0.8_fp
   MetState%TSKIN = 300._fp
   MetState%CLAYFRAC = 0.5_fp
   MetState%SANDFRAC = 0.5_fp
   MetState%GWETTOP = 0.01_fp
   MetState%USTAR = 0.5_fp
   MetState%USTAR_THRESHOLD = 0.1_fp
   allocate(MetState%AIRDEN(1))
   MetState%AIRDEN = 1.2_fp  ! kg/m3

   title = "Dust Test 1"
   call print_info(Config, MetState, title)

   call cc_dust_init(Config, DustState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      ErrMsg = 'Error in cc_dust_init'
      call cc_error( ErrMsg, rc, thisLoc )
      stop 1
   end if
   DustState%AlphaScaleFactor = 1.0_fp

   call cc_dust_run(MetState, DiagState, DustState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      ErrMsg = 'Error in cc_dust_run'
      call cc_error( ErrMsg, rc, thisLoc )
      stop 1
   end if
   ! Expected Result = 2.78159837E-06
   call assert (DiagState%dust_total_flux > 2780.0_fp, "Test Fengsha Dust Scheme")

   title = "Dust Test 2 - ustar == ustar_threshold"
   MetState%USTAR = 0.1_fp

   call print_info(Config, MetState, title)

   call cc_dust_run(MetState, DiagState, DustState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      ErrMsg = 'Error in cc_dust_run'
      call cc_error( ErrMsg, rc, thisLoc )
      stop 1
   end if

   call assert(DiagState%dust_total_flux .eq. 0.0_fp, "Test 2 FENGSHA Dust Scheme (no Dust)")

   title = "Dust Test 3 - test horizontal flux change"
   MetState%z0 = .001_fp
   DustState%HorizFluxOpt = 2
   MetState%USTAR = 0.5_fp
   MetState%U10M = 5.0_fp
   MetState%V10M = 5.0_fp
   DiagState%dust_total_flux = 0.0_fp

   call print_info(Config, MetState, title)

   call cc_dust_run(MetState, DiagState, DustState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      ErrMsg = 'Error in cc_dust_run'
      call cc_error( ErrMsg, rc, thisLoc )
      stop 1
   end if

   call assert(DiagState%dust_total_flux > 1000.0_fp, "Test different horizontal flux")
   write(*,*) 'Test 3 Success!!!!!'

   call cc_dust_finalize(DustState, RC)
   if (RC /= CC_SUCCESS) then
      ErrMsg = 'Error in cc_dust_finalize'
      call cc_error( ErrMsg, rc, thisLoc )
      stop 1
   endif

   title = "Dust Test 4 - Change Dust Scheme"
   Config%dust_activate = .true.
   Config%dust_scheme = 2
   Config%dust_drag_opt = 1
   Config%dust_moist_opt = 1
   Config%dust_horizflux_opt = 1
   MetState%U10M = 5.0_fp
   MetState%V10M = 5.0_fp

   call print_info(Config, MetState, title)

   call cc_dust_init(Config, DustState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      ErrMsg = 'Error in cc_dust_init'
      call cc_error( ErrMsg, rc, thisLoc )
      stop 1
   end if

   call cc_dust_run(MetState, DiagState, DustState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      ErrMsg = 'Error in cc_dust_run'
      call cc_error( ErrMsg, rc, thisLoc )
      stop 1
   end if

   call assert(DiagState%dust_total_flux > 700.0_fp, "Test Ginoux Dust Scheme Success")

contains

   subroutine print_info(Config_, MetState_, title_)
      type(ConfigType), intent(in) :: Config_
      type(MetStateType), intent(in) :: MetState_
      character(len=*), intent(in) :: title_

      write(*,*) '======================================='
      write(*,*) title_
      write(*,*) '======================================='
      write(*,*) '*************'
      write(*,*) 'Configuration '
      write(*,*) '*************'
      write(*,*) 'DustState%activate = ', Config_%dust_activate
      write(*,*) 'DustState%dust_scheme = ', Config_%dust_scheme
      write(*,*) 'DustState%dust_moist_opt = ', Config_%dust_drag_opt
      write(*,*) 'DustState%dust_horizflux_opt = ', Config_%dust_moist_opt
      write(*,*) 'ChemState%nSpeciesDust = ', Config_%dust_horizflux_opt
      write(*,*) 'MetState%DSOILTYPE = ', MetState_%DSOILTYPE
      write(*,*) 'MetState%SSM = ', MetState_%SSM
      write(*,*) 'MetState%RDRAG = ', MetState_%RDRAG
      write(*,*) 'MetState%TSKIN =', MetState_%TSKIN
      write(*,*) 'MetState%CLAYFRAC =', MetState_%CLAYFRAC
      write(*,*) 'MetState%SANDFRAC =', MetState_%SANDFRAC
      write(*,*) 'MetState%GWETTOP =', MetState_%GWETTOP
      write(*,*) 'MetState%USTAR =', MetState_%USTAR
      write(*,*) 'MetState%USTAR_THRESHOLD =', MetState_%USTAR_THRESHOLD
      write(*,*) 'MetState%AIRDEN =', MetState_%AIRDEN

   end subroutine print_info

end program test_dust
