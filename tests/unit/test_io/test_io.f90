module test_io_mod

  use mod_xdmf, only: xdmf_contour_t
  use mod_kinds, only: ik, rk
  use mod_input, only: input_t
  use mod_grid, only: grid_t, grid_3d_t
  use mod_grid_factory, only: grid_factory_t

  implicit none

  private
  public :: test_1d_contour, test_2d_contour, test_3d_contour

contains

  subroutine test_1d_contour()
    class(xdmf_contour_t), allocatable :: contour
    type(input_t) :: input
    class(grid_factory_t), allocatable :: grid_factory
    class(grid_t), pointer :: grid => null()
    integer(ik) :: t = 0

    real(rk), dimension(:), allocatable :: dummy_data

    allocate(dummy_data(3))
    dummy_data = 1._rk

    input = input_t(nx=3, ny=4, nz=5, &
                    relaxation_time=1, characteristic_density=1, characteristic_pressure=1, &
                    lattice_type='d1q3')
    allocate(grid_factory_t :: grid_factory)
    grid => grid_factory%create_grid(input)

    grid%time = 0.0_rk

    grid%save_contour()

  end subroutine

  subroutine test_2d_contour()
    class(xdmf_contour_t), allocatable :: contour
    type(input_t) :: input
    class(grid_factory_t), allocatable :: grid_factory
    class(grid_t), pointer :: grid => null()
    integer(ik) :: t = 0

    real(rk), dimension(:, :), allocatable :: dummy_data

    allocate(dummy_data(3, 4))
    dummy_data(1, :) = 1._rk
    dummy_data(2, :) = 2._rk

    input = input_t(nx=3, ny=4, nz=5, &
                    relaxation_time=1, characteristic_density=1, characteristic_pressure=1, &
                    lattice_type='d2q9')
    allocate(grid_factory_t :: grid_factory)
    grid => grid_factory%create_grid(input)

    grid%time = 0.0_rk

  end subroutine

  subroutine test_3d_contour()
    class(xdmf_contour_t), allocatable :: contour
    type(input_t) :: input
    class(grid_factory_t), allocatable :: grid_factory
    class(grid_t), pointer :: grid => null()
    integer(ik) :: t = 0

    real(rk), dimension(:, :, :), allocatable :: dummy_data

    allocate(dummy_data(3, 4, 5))
    dummy_data(1, :, :) = 1._rk
    dummy_data(2, :, :) = 2._rk
    dummy_data(3, :, :) = 3._rk

    input = input_t(nx=3, ny=4, nz=5, &
                    relaxation_time=1, characteristic_density=1, characteristic_pressure=1, &
                    lattice_type='d3q19_em')
    allocate(grid_factory_t :: grid_factory)
    grid => grid_factory%create_grid(input)

    grid%time = 0.0_rk

  end subroutine

end module test_io_mod

program test_io

  use test_io_mod

  implicit none

  if(this_image() == 1) then
    call test_1d_contour()
    call test_2d_contour()
    call test_3d_contour()
  endif

end program test_io
