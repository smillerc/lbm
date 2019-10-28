module mod_sim_factory

  use mod_kinds, only: ik, rk
  use mod_input, only: input_t
  use mod_sim_type, only: base_sim_t
  use mod_sim_2d_type, only: sim_2d_t

  implicit none

  private
  public :: sim_factory_t

  type sim_factory_t
    integer(ik) :: dim
    class(base_sim_t), pointer :: sim_ptr => null()
  contains
    private
    procedure, public :: initialize
    procedure, public :: create_sim
    procedure, public :: finalize
  end type sim_factory_t

contains

  subroutine initialize(self, input)
    class(sim_factory_t), intent(inout) :: self
    class(input_t), intent(in) :: input
    self%sim_ptr => null()
  end subroutine initialize

  subroutine finalize(self)
    class(sim_factory_t), intent(inout) :: self
    deallocate(self%sim_ptr)
    nullify(self%sim_ptr)
  end subroutine finalize

  function create_sim(self, input) result(ptr)
    !< Factory function to create a pointer to a sim object

    class(sim_factory_t) :: self
    class(input_t), intent(in) :: input
    class(base_sim_t), pointer :: ptr

    if(associated(self%sim_ptr)) deallocate(self%sim_ptr)

    select case(input%dim)

      ! case (1)
      !   allocate(sim_1d_t :: self%sim_ptr)
      !   call self%sim_ptr%initialize(input)
      !   ptr => self%sim_ptr

    case(2)
      allocate(sim_2d_t :: self%sim_ptr)
      call self%sim_ptr%initialize(input)
      ptr => self%sim_ptr

      ! case (3)
      !   allocate(sim_2d_t :: self%sim_ptr)
      !   call self%sim_ptr%initialize(input)
      !   ptr => self%sim_ptr

    case default
      if(this_image() == 1) print *, "Error in "//__FILE__//", unsupported simulation dimension'"
      error stop
    end select

  end function create_sim

end module mod_sim_factory
