module mod_lattice_type

  use mod_kinds, only: ik, rk

  implicit none

  ! private
  ! public :: base_2d_lattice_t

  type, abstract :: base_lattice_t
    !> Abstract lattice type that all other concrete lattice types inherit from

    character(:), allocatable :: name
    integer(ik) :: n_weights
    integer(ik) :: dimension
    real(rk), dimension(:), allocatable :: f_eq !! Equilibrium distribution function
    real(rk), dimension(:), allocatable :: w_i  !! Lattice distribution weights
    real(rk), dimension(:, :), allocatable :: v_hat  !! Lattice velocities unit vectors
    integer(ik), dimension(:), allocatable :: neighbor_indices  !! Lattice velocities unit vectors

  contains
    procedure(init), public, deferred :: init
  end type base_lattice_t

  type, abstract, extends(base_lattice_t) :: base_2d_lattice_t
    integer(ik), dimension(:), allocatable :: ilow, ihigh
    integer(ik), dimension(:), allocatable :: jlow, jhigh
  end type base_2d_lattice_t

  abstract interface
    subroutine init(self)
      use mod_kinds, only: ik, rk
      import :: base_lattice_t
      class(base_lattice_t), intent(inout) :: self
    end subroutine init
  end interface

end module mod_lattice_type
