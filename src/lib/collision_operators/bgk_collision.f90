module mod_bgk_collision_operator
  !< Implementation for the classic single-time relaxation BGK collision operator

  use mod_kinds, only: rk
  use mod_input, only: input_t
  use mod_collision_operator, only: collision_operator_t

  implicit none

  private
  public :: bgk_collision_operator_t

  type, extends(collision_operator_t) :: bgk_collision_operator_t
    !< Single relaxation time BGK operator
    real(rk) :: tau !! relaxation_time
  contains
    procedure :: initialize => init_bgk
    procedure, public :: collide => bgk_collide
  end type bgk_collision_operator_t

contains

  subroutine init_bgk(self, input)
    class(bgk_collision_operator_t), intent(inout) :: self
    class(input_t), intent(in) :: input

    self%name = "Single Relaxation Time BGK"
    self%tau = input%relaxation_time

  end subroutine init_bgk

  subroutine bgk_collide(self)
    class(bgk_collision_operator_t), intent(in) :: self
  end subroutine

  ! elemental real(rk) function bgk_collide(self, f, f_eq) result(omega)

  !   ! Input
  !   class(bgk_collision_operator_t), intent(in) :: self
  !   real(rk), intent(in) :: f         !! Distribution function
  !   real(rk), intent(in) :: f_eq      !! Equilibrium distribution function

  !   omega = (-1.0_rk / self%tau) * (f - f_eq)
  ! end function bgk_collide

end module mod_bgk_collision_operator
