program lbm

  use mod_kinds, only: ik, rk
  use mod_input, only: input_t
  ! use mod_grid, only : grid_t
  ! use mod_grid_factory, only : grid_factory_t

  implicit none

  character(150) :: command_line_arg
  character(50) :: input_filename
  character(50) :: contour_filename
  class(input_t), allocatable :: input
  ! class(grid_factory_t), allocatable :: grid_factory
  ! class(grid_t), pointer :: grid => null()
  integer(ik) :: t = 0

  call get_command_argument(1, command_line_arg)
  input_filename = trim(command_line_arg)

  allocate(input_t :: input)
  call input%read(input_filename)

  ! allocate(grid_factory_t :: grid_factory)
  ! grid => grid_factory%create_grid(input)

  time_loop: do t = 1, input%nsteps

    ! call grid%compute_equilibrium()
    ! call grid%compute_macros()

    ! if (this_image() == 1 .and. mod(t, input%contour_interval) == 0) then
    !   call grid%save_contour()
    ! end if

    ! call grid%collide()
    ! call grid%stream()

  end do time_loop

end program lbm
