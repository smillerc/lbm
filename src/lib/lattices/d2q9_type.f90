module mod_d2q9_type
  !> Summary: Define the specifics of the D2Q9 lattice structure

  use mod_kinds, only: ik, rk
  use mod_lattice_type, only: base_2d_lattice_t

  implicit none
  ! private
  ! public :: d2q9_lattice_t

  type, extends(base_2d_lattice_t) :: d2q9_lattice_t
  contains
    procedure :: initialize => init_d2q9
  end type d2q9_lattice_t

contains
  subroutine init_d2q9(self)
    class(d2q9_lattice_t), intent(inout) :: self

    self%n_weights = 9
    self%name = 'D2Q9'
    self%dimension = 2

    allocate(self%f_eq(0:self%n_weights - 1))
    self%f_eq = 0.0_rk

    allocate(self%w_i(0:self%n_weights - 1))
    self%w_i(0) = 4.0_rk / 9.0_rk
    self%w_i(1:4) = 1.0_rk / 9.0_rk
    self%w_i(5:8) = 1.0_rk / 36.0_rk

    allocate(self%v_hat(0:self%n_weights - 1, self%dimension))
    self%v_hat(0, :) = [0, 0]
    self%v_hat(1, :) = [1, 0]
    self%v_hat(2, :) = [0, 1]
    self%v_hat(3, :) = [-1, 0]
    self%v_hat(4, :) = [0, -1]
    self%v_hat(5, :) = [1, 1]
    self%v_hat(6, :) = [-1, 1]
    self%v_hat(7, :) = [-1, -1]
    self%v_hat(8, :) = [1, -1]

    allocate(self%neighbor_indices(1:self%n_weights - 1))
    self%neighbor_indices(1) = 3
    self%neighbor_indices(2) = 4
    self%neighbor_indices(3) = 1
    self%neighbor_indices(4) = 2
    self%neighbor_indices(5) = 7
    self%neighbor_indices(6) = 8
    self%neighbor_indices(7) = 5
    self%neighbor_indices(8) = 6

  end subroutine init_d2q9

end module mod_d2q9_type
