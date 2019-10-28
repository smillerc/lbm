module test_grid_factory_mod
  use mod_kinds, only: ik, rk
  use mod_input, only: input_t
  use mod_grid, only: grid_t, grid_3d_t
  use mod_grid_two_fluid, only: two_fluid_grid_3d_t
  use mod_grid_factory, only: grid_factory_t

  implicit none

  ! type(input_t) :: input
  class(grid_t), pointer :: grid => null()

  ! Test parameters

  ! Grid size
  integer(ik), parameter :: nx = 3
  integer(ik), parameter :: ny = 4
  integer(ik), parameter :: nz = 5
  integer(ik), parameter :: spacing = 1
  integer(ik), parameter :: relaxation_time = 1
  integer(ik), parameter :: characteristic_density = 1
  integer(ik), parameter :: characteristic_pressure = 1
  character(len=8), parameter :: lattice_type = "d3q19_em"

contains

  subroutine test_x_only()
    type(input_t) :: input
    class(grid_factory_t), allocatable :: grid_factory

    if(this_image() == 1) print *, "Testing x only input..."
    input = input_t(nx=nx, spacing=1, &
                    relaxation_time=1, characteristic_density=1, characteristic_pressure=1, &
                    lattice_type='d3q19_em')

    if(allocated(grid_factory)) deallocate(grid_factory)
    allocate(grid_factory_t :: grid_factory)
    grid => grid_factory%create_grid(input)

    if(grid%shape(1) /= nx .or. &
       grid%shape(2) /= 1 .or. &
       grid%shape(3) /= 1) then

      error stop "Test failed, grid dimensions should be 3 1 1"
    end if

    if(this_image() == 1) print *, "Success"
    deallocate(grid_factory)
  end subroutine test_x_only

  subroutine test_y_only()
    type(input_t) :: input
    class(grid_factory_t), allocatable :: grid_factory

    if(this_image() == 1) print *, "Testing y only input..."
    input = input_t(ny=ny, spacing=1, &
                    relaxation_time=1, characteristic_density=1, characteristic_pressure=1, &
                    lattice_type='d3q19_em')

    if(allocated(grid_factory)) deallocate(grid_factory)
    allocate(grid_factory_t :: grid_factory)
    grid => grid_factory%create_grid(input)

    if(grid%shape(1) /= 1 .or. &
       grid%shape(2) /= ny .or. &
       grid%shape(3) /= 1) then

      error stop "Test failed, grid dimensions should be 3 1 1"
    end if

    if(this_image() == 1) print *, "Success"
    deallocate(grid_factory)
  end subroutine test_y_only

  subroutine test_z_only()
    type(input_t) :: input
    class(grid_factory_t), allocatable :: grid_factory

    if(this_image() == 1) print *, "Testing z only input..."
    input = input_t(nz=nz, spacing=1, &
                    relaxation_time=1, characteristic_density=1, characteristic_pressure=1, &
                    lattice_type='d3q19_em')

    if(allocated(grid_factory)) deallocate(grid_factory)
    allocate(grid_factory_t :: grid_factory)
    grid => grid_factory%create_grid(input)

    if(grid%shape(1) /= 1 .or. &
       grid%shape(2) /= 1 .or. &
       grid%shape(3) /= nz) then

      error stop "Test failed, grid dimensions should be 3 1 1"
    end if

    if(this_image() == 1) print *, "Success"
    deallocate(grid_factory)
  end subroutine test_z_only

  subroutine test_wrong_lattice_for_grid()
  end subroutine

end module test_grid_factory_mod

program test_grid_factory

  use test_grid_factory_mod
  implicit none

  if(this_image() == 1) print *, "Testing grid_factory..."

  call test_x_only()
  call test_y_only()
  call test_z_only()

  if(this_image() == 1) print *, "Success"

end program test_grid_factory
