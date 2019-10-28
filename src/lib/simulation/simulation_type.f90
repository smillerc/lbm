module mod_sim_type

  use mod_kinds, only: ik, rk
  use mod_input, only: input_t

  implicit none

  type, abstract :: base_sim_t
    integer(ik), public :: n_species
    integer(ik), public :: n_fluids
    integer(ik), public :: dimension
  contains
    procedure(initialize), deferred :: initialize
  end type base_sim_t

  abstract interface
    subroutine initialize(self, input)
      import base_sim_t
      import input_t
      class(base_sim_t), intent(inout) :: self
      class(input_t), intent(in) :: input

    end subroutine
  end interface

end module
