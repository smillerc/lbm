module mod_sim_2d_type

  use mod_kinds, only: ik, rk
  use mod_input, only: input_t
  use mod_sim_type, only: base_sim_t

  implicit none

  private
  public :: sim_2d_t

  type, extends(base_sim_t) :: sim_2d_t
  contains
    procedure, public :: initialize => initialize_fluid_2d
  end type sim_2d_t

contains
  subroutine initialize_fluid_2d(self, input)
    class(sim_2d_t), intent(inout) :: self
    class(input_t), intent(in) :: input

    self%n_species = 1
    self%n_fluids = 1
    self%dimension = 2
  end subroutine initialize_fluid_2d

end module mod_sim_2d_type
