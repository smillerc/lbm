module mod_collision_operator
  !< Framework for collision operators. Each collision operator implementation must
  !< inherit from this type

  use mod_input, only: input_t

  implicit none

  private
  public :: collision_operator_t

  type, abstract :: collision_operator_t
    !< Abstract collision operator type
    character(:), allocatable :: name
  contains
    procedure(initialize), public, deferred :: initialize
    procedure(collide), public, deferred :: collide
  end type collision_operator_t

  abstract interface
    subroutine initialize(self, input)
      import input_t
      import collision_operator_t
      class(input_t), intent(in) :: input
      class(collision_operator_t), intent(inout) :: self
    end subroutine initialize

    subroutine collide(self)
      import collision_operator_t
      class(collision_operator_t), intent(in) :: self
    end subroutine collide

  end interface

end module mod_collision_operator
