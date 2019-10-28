module mod_io
  !! Summary: Define the abstract IO and factory methods
  use mod_kinds, only: ik, rk

  implicit none

  private
  public :: contour_writer_t

  type, abstract :: contour_writer_t
    !! Summary: The contour writer lives inside the grid object.

    private
    !! The abstract skeleton that ALL contour writers need to inherit from
    character(:), allocatable, public :: format
    character(:), allocatable, public :: filename
    character(5), public :: filename_prefix = 'step_'

    integer(ik), public :: timestep
    real(rk), public :: time
    integer(ik), public :: ndim
    integer(ik), allocatable, dimension(:), public :: shape
  contains
    private
    procedure(initialize), deferred, public :: initialize
    procedure(add_header), deferred, public :: add_header
    procedure(finalize), deferred, public :: finalize

    generic, public :: add_data => add_int_1d, add_int_2d, add_int_3d, &
      add_real64_1d, add_real64_2d, add_real64_3d

    procedure(add_int_1d), deferred, private :: add_int_1d
    procedure(add_int_2d), deferred, private :: add_int_2d
    procedure(add_int_3d), deferred, private :: add_int_3d
    procedure(add_real64_1d), deferred, private :: add_real64_1d
    procedure(add_real64_2d), deferred, private :: add_real64_2d
    procedure(add_real64_3d), deferred, private :: add_real64_3d

  end type contour_writer_t

  abstract interface
    subroutine initialize(self, timestep, time, nx_ny_nz, ndim)
      import :: contour_writer_t
      import :: rk
      import :: ik
      class(contour_writer_t), intent(inout) :: self
      real(rk), intent(in) :: time
      integer(ik), intent(in) :: timestep
      integer(ik), intent(in) :: nx_ny_nz(3)
      integer(ik), intent(in) :: ndim
    end subroutine initialize

    subroutine finalize(self)
      import :: contour_writer_t
      class(contour_writer_t), intent(inout) :: self
    end subroutine finalize

    subroutine add_header(self)
      import :: contour_writer_t
      class(contour_writer_t), intent(inout) :: self
    end subroutine add_header

    subroutine add_int_1d(self, name, data, units, description)
      import :: contour_writer_t
      import :: ik
      class(contour_writer_t), intent(inout) :: self
      integer(ik), dimension(:), intent(in):: data
      character(*), intent(in) :: name
      character(*), intent(in) :: units
      character(*), intent(in) :: description
    end subroutine add_int_1d

    subroutine add_int_2d(self, name, data, units, description)
      import :: contour_writer_t
      import :: ik
      class(contour_writer_t), intent(inout) :: self
      integer(ik), dimension(:, :), intent(in):: data
      character(*), intent(in) :: name
      character(*), intent(in) :: units
      character(*), intent(in) :: description
    end subroutine add_int_2d

    subroutine add_int_3d(self, name, data, units, description)
      import :: contour_writer_t
      import :: ik
      class(contour_writer_t), intent(inout) :: self
      integer(ik), dimension(:, :, :), intent(in):: data
      character(*), intent(in) :: name
      character(*), intent(in) :: units
      character(*), intent(in) :: description
    end subroutine add_int_3d

    subroutine add_real64_1d(self, name, data, units, description)
      import :: contour_writer_t
      import :: rk
      class(contour_writer_t), intent(inout) :: self
      real(rk), dimension(:), intent(in) :: data
      character(*), intent(in) :: name
      character(*), intent(in) :: units
      character(*), intent(in) :: description
    end subroutine add_real64_1d

    subroutine add_real64_2d(self, name, data, units, description)
      import :: contour_writer_t
      import :: rk
      class(contour_writer_t), intent(inout) :: self
      real(rk), dimension(:, :), intent(in) :: data
      character(*), intent(in) :: name
      character(*), intent(in) :: units
      character(*), intent(in) :: description
    end subroutine add_real64_2d

    subroutine add_real64_3d(self, name, data, units, description)
      import :: contour_writer_t
      import :: rk
      class(contour_writer_t), intent(inout) :: self
      real(rk), dimension(:, :, :), intent(in) :: data
      character(*), intent(in) :: name
      character(*), intent(in) :: units
      character(*), intent(in) :: description
    end subroutine add_real64_3d
  end interface

contains

end module mod_io
