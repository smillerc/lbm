module mod_io_factory
  !! Summary: Define the abstract IO and factory methods
  use mod_kinds, only: ik, rk
  use mod_io, only: contour_writer_t
  use mod_xdmf, only: xdmf_contour_t
  use mod_input, only: input_t
  use mod_sim_type, only: base_sim_t

  implicit none

  private
  public :: io_factory_t

  type io_factory_t
    class(contour_writer_t), pointer :: contour_writer_ptr => null()
    character(:), allocatable :: format
  contains
    private
    procedure, public :: initialize
    procedure, public :: create
    procedure, public :: finalize
  end type io_factory_t

contains

  subroutine initialize(self, format)
    class(io_factory_t), intent(inout) :: self
    character(*), intent(in) :: format

    self%format = trim(format)
    self%contour_writer_ptr => null()
  end subroutine

  subroutine finalize(self)
    class(io_factory_t) :: self
    deallocate(self%contour_writer_ptr)
  end subroutine

  function create(self, timestep, time, nx_ny_nz, ndim) result(ptr)
    !! Factory function to create a pointer to a contour writer

    class(io_factory_t), intent(inout) :: self
    class(contour_writer_t), pointer :: ptr
    integer(ik), dimension(:), intent(in) :: nx_ny_nz
    integer(ik), intent(in) :: ndim
    real(rk), intent(in) :: time
    integer(ik), intent(in) :: timestep

    select case(self%format)

    case('xdmf')
      allocate(xdmf_contour_t :: ptr)
      call self%contour_writer_ptr%initialize(timestep, time, nx_ny_nz, ndim)
      ptr => self%contour_writer_ptr

    case default
      if(this_image() == 1) error stop "Error in "//__FILE__// &
        ", unsupported contour format type type: '"//self%format//"'"
    end select

  end function create

end module mod_io_factory
