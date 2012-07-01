! gas_physics_fun.f90 - a unit test suite for gas_physics.f90
!
! funit generated this file from gas_physics.fun

module gas_physics_fun

 use gas_physics

 implicit none

 logical :: noAssertFailed

 public :: test_gas_physics

 private

 integer :: numTests          = 0
 integer :: numAsserts        = 0
 integer :: numAssertsTested  = 0
 integer :: numFailures       = 0



 contains

 subroutine viscosity_varies_as_temperature

  ! Assert_Real_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.( (      0.0 &
        +2*spacing(real(      0.0)) ) &
        .ge. &
        (viscosity(0.0) ) &
            .and. &
     (      0.0 &
      -2*spacing(real(      0.0)) ) &
      .le. &
       (viscosity(0.0) ) )) then
      print *, " *Assert_Real_Equal failed* in test viscosity_varies_as_temperature &
              &[gas_physics.fun:4]"
      print *, "  ", "viscosity(0.0)  (", &
 viscosity(0.0) , &
  ") is not", &
       0.0,&
 "within", &
  2*spacing(real(      0.0))
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((viscosity(50.0) &
     +1e-3 ) &
     .ge. &
     ( 0.7071) &
             .and. &
     (viscosity(50.0) &
     -1e-3 ) &
     .le. &
     ( 0.7071) )) then
      print *, " *Assert_Equal_Within failed* in test viscosity_varies_as_temperature &
              &[gas_physics.fun:5]"
      print *, "  ", " 0.7071 (", 0.7071,") is not", &
 viscosity(50.0),"within",1e-3 
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine viscosity_varies_as_temperature


 subroutine funit_setup
  noAssertFailed = .true.
 end subroutine funit_setup


 subroutine funit_teardown
 end subroutine funit_teardown


 subroutine test_gas_physics( nTests, nAsserts, nAssertsTested, nFailures )

  integer :: nTests
  integer :: nAsserts
  integer :: nAssertsTested
  integer :: nFailures

  continue

  call funit_setup
  call viscosity_varies_as_temperature
  call funit_teardown

  nTests          = numTests
  nAsserts        = numAsserts
  nAssertsTested  = numAssertsTested
  nFailures       = numFailures

 end subroutine test_gas_physics

end module gas_physics_fun
