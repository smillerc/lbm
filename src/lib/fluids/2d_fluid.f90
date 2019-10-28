module mod_2d_fluid

  use mod_kinds, only: ik, rk
  use mod_input, only: input_t
  use mod_fluid, only: fluid_t

  implicit none

  private
  public :: fluid_2d_t

  type, extends(fluid_t) :: fluid_2d_t
    private
    real(rk), public, dimension(2) :: external_force
    real(rk), public, allocatable, dimension(:, :, :) :: particle_populations
    real(rk), public, allocatable, dimension(:, :, :) :: equillibrium_particle_populations

    real(rk), public, allocatable, dimension(:, :) :: density
    real(rk), public, allocatable, dimension(:, :) :: temperature
    real(rk), public, allocatable, dimension(:, :) :: pressure
    real(rk), public, allocatable, dimension(:, :, :) :: velocity

    integer(ik), public :: i_low, i_high
    integer(ik), public :: j_low, j_high
  contains
    procedure, public :: initialize => initialize_fluid_2d
    procedure, public :: compute_macros => compute_macros_fluid_2d
    procedure, public :: compute_equilibrium => compute_equlibrium_fluid_2d
    procedure, public :: stream => stream_fluid_2d
    procedure, public :: collide => collide_fluid_2d
    procedure, public :: finalize => finalize_fluid_2d
  end type

contains

  subroutine initialize_fluid_2d(self, input)
    class(fluid_2d_t), intent(inout) :: self
    class(input_t), intent(in) :: input

    call self%get_collision_operator(input)

  end subroutine

  subroutine get_grid_extents_fluid_2d(self)
    class(fluid_2d_t), intent(inout) :: self
  end subroutine

  subroutine compute_macros_fluid_2d(self)
    class(fluid_2d_t), intent(inout) :: self

    ! call self%compute_density_fluid_2d()
    ! call self%compute_velocity_fluid_2d()
  end subroutine

  subroutine compute_density_fluid_2d(self)
    class(fluid_2d_t), intent(inout) :: self

    ! self%density = sum(self%f,dim=3)

  end subroutine

  subroutine compute_velocity_fluid_2d(self)
    class(fluid_2d_t), intent(inout) :: self

    ! integer(ik) :: i, j, l

    ! do concurrent (i = self%i_start : self%i_end)
    !   do concurrent (j = self%j_start : self%j_end)

    !       associate(rho => self%density(i,j), &
    !                 e => self%lattice%v_hat, &
    !                 f => self%f(i,j,:), &
    !                 u => self%velocity(i,j,:))

    !         u(1) = (1 / rho) * sum(f * e(1,:))
    !         u(2) = (1 / rho) * sum(f * e(2,:))

    !       end associate
    !     end do
    !   end do

  end subroutine

  subroutine compute_equlibrium_fluid_2d(self)
    class(fluid_2d_t), intent(inout) :: self

    ! integer(ik) :: i, j, l

    ! do concurrent (i = self%i_start : self%i_end)
    !   do concurrent (j = self%j_start : self%j_end)

    !       associate(rho => self%density(i,j), &
    !                 f_eq => self%f_eq(i,j) )

    !         f_eq

    !       end associate
    !     end do
    !   end do

  end subroutine

  subroutine stream_fluid_2d(self)
    class(fluid_2d_t), intent(inout) :: self
  end subroutine

  subroutine collide_fluid_2d(self)
    class(fluid_2d_t), intent(inout) :: self
  end subroutine

  subroutine finalize_fluid_2d(self)
    class(fluid_2d_t), intent(inout) :: self

    deallocate(self%particle_populations)
    deallocate(self%equillibrium_particle_populations)
    deallocate(self%density)
    deallocate(self%temperature)
    deallocate(self%pressure)
    deallocate(self%velocity)

  end subroutine

end module mod_2d_fluid
