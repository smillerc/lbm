module mod_input

  use mod_kinds, only: ik, rk
  use cfgio_mod, only: cfg_t, parse_cfg

  implicit none

  private
  public :: input_t

  type input_t
    ! general
    character(:), allocatable :: title

    ! grid
    integer(ik) :: nx = 1
    integer(ik) :: ny = 1
    integer(ik) :: nz = 1
    integer(ik) :: dim = 1
    integer(ik) :: spacing = 1

    ! boundary
    character(:), allocatable ::  plus_x_bc
    character(:), allocatable :: minus_x_bc

    character(:), allocatable ::  plus_y_bc
    character(:), allocatable :: minus_y_bc

    character(:), allocatable ::  plus_z_bc
    character(:), allocatable :: minus_z_bc

    ! timing
    integer(ik) :: nsteps = 1
    integer(ik) :: contour_interval = -1

    ! io
    character(:), allocatable :: contour_io_format

    ! physics
    real(rk) :: relaxation_time
    real(rk) :: characteristic_density
    real(rk) :: characteristic_pressure
    real(rk) :: polytropic_index = 5.0_rk / 3.0_rk

    ! lattice
    character(:), allocatable :: lattice_type
  contains
    procedure, public :: read
  end type input_t

contains

  subroutine read(self, filename)

    character(len=*), intent(in) :: filename
    class(input_t), intent(inout) :: self

    character(len=120) :: char_buffer
    type(cfg_t) :: cfg
    logical :: file_exists

    file_exists = .false.

    if(this_image() == 1) then
      print *, 'Reading input file: '//trim(filename)
    end if

    inquire(file=filename, EXIST=file_exists)
    if(.not. file_exists) error stop "Error: Input file not found"

    cfg = parse_cfg(trim(filename))

    ! General
    call cfg%get("general", "title", char_buffer)
    self%title = trim(char_buffer)
    call cfg%get("general", "nsteps", self%nsteps)
    call cfg%get("general", "contour_interval", self%contour_interval)

    ! Lattice
    call cfg%get("lattice", "type", char_buffer, 'd3q19_em')
    self%lattice_type = trim(char_buffer)

    ! Physics
    call cfg%get("physics", "relaxation_time", self%relaxation_time)
    call cfg%get("physics", "characteristic_density", self%characteristic_density)
    call cfg%get("physics", "characteristic_pressure", self%characteristic_pressure)
    call cfg%get("physics", "polytropic_index", self%polytropic_index)

    ! Grid
    call cfg%get("grid", "nx", self%nx)
    call cfg%get("grid", "ny", self%ny, 1)
    call cfg%get("grid", "nz", self%nz, 1)
    call cfg%get("grid", "dim", self%dim)

    ! boundary conditions
    call cfg%get("boundary_conditions", "plus_x", char_buffer, 'periodic')
    self%plus_x_bc = trim(char_buffer)
    call cfg%get("boundary_conditions", "minus_x", char_buffer, 'periodic')
    self%minus_x_bc = trim(char_buffer)

    call cfg%get("boundary_conditions", "plus_y", char_buffer, 'periodic')
    self%plus_y_bc = trim(char_buffer)
    call cfg%get("boundary_conditions", "minus_y", char_buffer, 'periodic')
    self%minus_y_bc = trim(char_buffer)

    call cfg%get("boundary_conditions", "plus_z", char_buffer, 'periodic')
    self%plus_z_bc = trim(char_buffer)
    call cfg%get("boundary_conditions", "minus_z", char_buffer, 'periodic')
    self%minus_z_bc = trim(char_buffer)

    ! Input/Output
    call cfg%get("io", "format", char_buffer, 'xdmf')
    self%contour_io_format = trim(char_buffer)

  end subroutine read

end module mod_input
