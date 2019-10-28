module mod_collision_operator_factory

  use mod_kinds, only: ik, rk
  use mod_input, only: input_t
  use mod_collision_operator, only: collision_operator_t
  use mod_bgk_collision_operator, only: bgk_collision_operator_t
  use mod_particles_on_demand_collsion_operator, only: particles_on_demand_collision_operator_t

  implicit none

  private
  public :: collision_operator_factory_t

  type collision_operator_factory_t
    class(collision_operator_t), pointer :: collision_operator_ptr => null()
  contains
    private
    procedure, public :: initialize
    procedure, public :: create_collision_operator
    procedure, public :: finalize
  end type collision_operator_factory_t

contains

  subroutine initialize(self, input)
    class(collision_operator_factory_t), intent(inout) :: self
    class(input_t), intent(in) :: input
    self%collision_operator_ptr => null()
  end subroutine initialize

  subroutine finalize(self)
    class(collision_operator_factory_t), intent(inout) :: self
    deallocate(self%collision_operator_ptr)
    nullify(self%collision_operator_ptr)
  end subroutine finalize

  function create_collision_operator(self, input) result(ptr)
    !< Factory function to create a pointer to a collision_operator object

    class(collision_operator_factory_t) :: self
    class(input_t), intent(in) :: input
    class(collision_operator_t), pointer :: ptr

    if(associated(self%collision_operator_ptr)) deallocate(self%collision_operator_ptr)

    select case(trim(input%lattice_scheme))

    case("bgk")
      allocate(bgk_collision_operator_t :: self%collision_operator_ptr)
      call self%collision_operator_ptr%initialize(input)
      ptr => self%collision_operator_ptr

    case default
      if(this_image() == 1) error stop "Error in "//__FILE__// &
        ", unsupported collision operator: '"//trim(input%lattice_scheme)//"'"
    end select

  end function create_collision_operator

end module mod_collision_operator_factory
