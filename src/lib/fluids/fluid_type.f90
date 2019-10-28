module mod_fluid

  use mod_input, only: input_t
  use mod_collision_operator, only: collision_operator_t
  use mod_collision_operator_factory, only: collision_operator_factory_t

  implicit none

  private
  public :: fluid_t

  class(collision_operator_factory_t), allocatable :: collision_factory

  type, abstract :: fluid_t
    character(:), allocatable :: name
    class(collision_operator_t), pointer :: collision_operator

  contains
    procedure(initialize), public, deferred :: initialize
    procedure(finalize), public, deferred :: finalize
    procedure(compute_macros), public, deferred :: compute_macros
    procedure(compute_equilibrium), public, deferred :: compute_equilibrium
    procedure(stream), public, deferred :: stream
    procedure(collide), public, deferred :: collide
    procedure, public :: get_collision_operator
  end type

  abstract interface
    subroutine initialize(self, input)
      import input_t
      import fluid_t
      class(input_t), intent(in) :: input
      class(fluid_t), intent(inout) :: self
    end subroutine

    subroutine finalize(self)
      import fluid_t
      class(fluid_t), intent(inout) :: self
    end subroutine

    subroutine compute_macros(self)
      import :: fluid_t
      class(fluid_t), intent(inout) :: self
    end subroutine compute_macros

    subroutine compute_equilibrium(self)
      import :: fluid_t
      class(fluid_t), intent(inout) :: self
    end subroutine compute_equilibrium

    subroutine stream(self)
      import :: fluid_t
      class(fluid_t), intent(inout) :: self
    end subroutine stream

    subroutine collide(self)
      import :: fluid_t
      class(fluid_t), intent(inout) :: self
    end subroutine collide

  end interface

contains
  subroutine get_collision_operator(self, input)
    class(fluid_t), intent(inout) :: self
    class(input_t), intent(in) :: input

    allocate(collision_operator_factory_t :: collision_factory)
    self%collision_operator => collision_factory%create_collision_operator(input)
  end subroutine get_collision_operator

end module mod_fluid
