module test_two_fluid_grid_mod

  use mod_kinds, only: ik, rk
  use mod_input, only: input_t
  use mod_grid, only: grid_t, grid_3d_t
  use mod_grid_two_fluid, only: two_fluid_grid_3d_t
  use mod_grid_factory, only: grid_factory_t

  implicit none

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

  subroutine test_grid_extents(grid)
    class(grid_t), intent(in) :: grid

    if(this_image() == 1) print *, "Testing grid extents..."

    if(grid%shape(1) /= nx .or. grid%shape(2) /= ny .or. grid%shape(3) /= nz) then
      error stop "Test failed, grid dimensions should be 3 4 5"
    end if

    if(this_image() == 1) print *, "Success"

  end subroutine test_grid_extents

  subroutine test_grid_initialization(grid)
    class(two_fluid_grid_3d_t), intent(in) :: grid

    if(this_image() == 1) print *, "Testing grid initialization..."

    if(all(grid%x < tiny(1.0_rk))) then
      error stop "Test failed, grid%x is all zeros"
    end if

    if(all(grid%y < tiny(1.0_rk))) then
      error stop "Test failed, grid%y is all zeros"
    end if

    if(all(grid%z < tiny(1.0_rk))) then
      error stop "Test failed, grid%z is all zeros"
    end if

    if(this_image() == 1) print *, "Success"
  end subroutine test_grid_initialization

  subroutine test_grid_type(grid)
    class(grid_t), intent(in) :: grid

    if(this_image() == 1) print *, "Testing grid type..."

    select type(grid)
    class is(two_fluid_grid_3d_t)
    class default
      error stop "Test failed, grid should be two_fluid_grid_3d_t"
    end select

    if(this_image() == 1) print *, "Success"

  end subroutine test_grid_type

  subroutine test_component_extents(grid)
    !! Test to make sure that the ion, electron,
    !! and em field all have the same dimensions
    class(two_fluid_grid_3d_t), intent(in) :: grid

    if(this_image() == 1) print *, "Testing grid extents..."

    ! EM Field
    associate(em=>grid%em_field)
      if(em%get_i_start() /= grid%i_start .or. &
         em%get_i_end() /= grid%i_end .or. &
         em%get_j_start() /= grid%j_start .or. &
         em%get_j_end() /= grid%j_end .or. &
         em%get_k_start() /= grid%k_start .or. &
         em%get_k_end() /= grid%k_end) then

        if(this_image() == 1) then
          print *, "Test test_component_extents failed, grid%em_field has a different shape than grid"
          write(*, '(2(a, 1x, i0, 1x))') "em%i_start", em%get_i_start(), "grid%i_start", grid%i_start
          write(*, '(2(a, 1x, i0, 1x))') "em%i_end", em%get_i_end(), "grid%i_end", grid%i_end
          write(*, '(2(a, 1x, i0, 1x))') "em%j_start", em%get_j_start(), "grid%j_start", grid%j_start
          write(*, '(2(a, 1x, i0, 1x))') "em%j_end", em%get_j_end(), "grid%j_end", grid%j_end
          write(*, '(2(a, 1x, i0, 1x))') "em%k_start", em%get_k_start(), "grid%k_start", grid%k_start
          write(*, '(2(a, 1x, i0, 1x))') "em%k_end", em%get_k_end(), "grid%k_end", grid%k_end
        end if
        error stop

      end if
    end associate

    ! Ion fluid
    associate(ion=>grid%ion_fluid)
      if(ion%get_i_start() /= grid%i_start .or. &
         ion%get_i_end() /= grid%i_end .or. &
         ion%get_j_start() /= grid%j_start .or. &
         ion%get_j_end() /= grid%j_end .or. &
         ion%get_k_start() /= grid%k_start .or. &
         ion%get_k_end() /= grid%k_end) then

        if(this_image() == 1) then
          print *, "Test test_component_extents failed, grid%em_field has a different shape than grid"
          write(*, '(2(a, 1x, i0, 1x))') "ion%i_start", ion%get_i_start(), "grid%i_start", grid%i_start
          write(*, '(2(a, 1x, i0, 1x))') "ion%i_end", ion%get_i_end(), "grid%i_end", grid%i_end
          write(*, '(2(a, 1x, i0, 1x))') "ion%j_start", ion%get_j_start(), "grid%j_start", grid%j_start
          write(*, '(2(a, 1x, i0, 1x))') "ion%j_end", ion%get_j_end(), "grid%j_end", grid%j_end
          write(*, '(2(a, 1x, i0, 1x))') "ion%k_start", ion%get_k_start(), "grid%k_start", grid%k_start
          write(*, '(2(a, 1x, i0, 1x))') "ion%k_end", ion%get_k_end(), "grid%k_end", grid%k_end
        end if
        error stop

      end if
    end associate

    ! Electron fluid
    associate(electron=>grid%electron_fluid)
      if(electron%get_i_start() /= grid%i_start .or. &
         electron%get_i_end() /= grid%i_end .or. &
         electron%get_j_start() /= grid%j_start .or. &
         electron%get_j_end() /= grid%j_end .or. &
         electron%get_k_start() /= grid%k_start .or. &
         electron%get_k_end() /= grid%k_end) then

        if(this_image() == 1) then
          print *, "Test test_component_extents failed, grid%em_field has a different shape than grid"
          write(*, '(2(a, 1x, i0, 1x))') "electron%i_start", electron%get_i_start(), "grid%i_start", grid%i_start
          write(*, '(2(a, 1x, i0, 1x))') "electron%i_end", electron%get_i_end(), "grid%i_end", grid%i_end
          write(*, '(2(a, 1x, i0, 1x))') "electron%j_start", electron%get_j_start(), "grid%j_start", grid%j_start
          write(*, '(2(a, 1x, i0, 1x))') "electron%j_end", electron%get_j_end(), "grid%j_end", grid%j_end
          write(*, '(2(a, 1x, i0, 1x))') "electron%k_start", electron%get_k_start(), "grid%k_start", grid%k_start
          write(*, '(2(a, 1x, i0, 1x))') "electron%k_end", electron%get_k_end(), "grid%k_end", grid%k_end
        end if
        error stop

      end if
    end associate

    if(this_image() == 1) print *, "Success"

  end subroutine test_component_extents

  subroutine test_wrong_input()
  end subroutine test_wrong_input

end module test_two_fluid_grid_mod

program test_grid

  use mod_kinds, only: ik, rk
  use mod_input, only: input_t
  use mod_grid, only: grid_t, grid_3d_t
  use mod_grid_two_fluid, only: two_fluid_grid_3d_t

  use test_two_fluid_grid_mod

  implicit none

  type(input_t) :: input
  class(two_fluid_grid_3d_t), pointer :: grid => null()

  if(this_image() == 1) then
    print *, "Testing grid..."
  end if

  input = input_t(nx=3, ny=4, nz=5, spacing=1, &
                  relaxation_time=1, characteristic_density=1, characteristic_pressure=1, &
                  lattice_type='d3q19_em')

  allocate(two_fluid_grid_3d_t :: grid)
  call grid%initialize(input)

  call test_grid_extents(grid)
  call test_grid_type(grid)
  call test_component_extents(grid)
  call test_grid_initialization(grid)

  print *, 'Success'

end program test_grid
