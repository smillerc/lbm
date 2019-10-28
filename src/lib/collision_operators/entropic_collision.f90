module mod_entropic_bgk_collision_operator
  !< Implementation for the classic single-time relaxation entropic_bgk collision operator

  use mod_kinds, only: rk
  use mod_input, only: input_t
  use mod_collision_operator, only: collision_operator_t

  implicit none

  private
  public :: entropic_bgk_collision_operator_t

  type, extends(collision_operator_t) :: entropic_bgk_collision_operator_t
    !< Single relaxation time entropic_bgk operator
    real(rk) :: tau !! relaxation_time
  contains
    procedure :: initialize => init_entropic_bgk
    procedure, public :: collide => entropic_bgk_collide
  end type entropic_bgk_collision_operator_t

contains

  subroutine init_entropic_bgk(self, input)
    class(entropic_bgk_collision_operator_t), intent(inout) :: self
    class(input_t), intent(in) :: input

    self%name = "Entropic BGK"
    self%tau = input%relaxation_time

  end subroutine init_entropic_bgk

  subroutine entropic_bgk_collide(self)
    class(entropic_bgk_collision_operator_t), intent(in) :: self
  end subroutine

  ! elemental real(rk) function entropic_bgk_collide(self, f, f_eq) result(omega)

  !   ! Input
  !   class(entropic_bgk_collision_operator_t), intent(in) :: self
  !   real(rk), intent(in) :: f         !! Distribution function
  !   real(rk), intent(in) :: f_eq      !! Equilibrium distribution function

  !   omega = (-1.0_rk / self%tau) * (f - f_eq)
  ! end function entropic_bgk_collide

end module mod_entropic_bgk_collision_operator
