module mod_particles_on_demand_collsion_operator
  !< Implementation for the Particles on Demain collision operator.
  !< Refer to DOI: 10.1103/PhysRevLett.121.130602

  use mod_kinds, only: rk
  use mod_input, only: input_t
  use mod_collision_operator, only: collision_operator_t

  implicit none

  private
  public :: particles_on_demand_collision_operator_t

  type, extends(collision_operator_t) :: particles_on_demand_collision_operator_t
    !< Particles on Demand operator
    real(rk), allocatable, dimension(:) :: weights
    real(rk) :: relaxation_parameter
  contains
    procedure :: initialize => init_particles_on_demand
    procedure, public :: collide => particles_on_demand_collide
  end type particles_on_demand_collision_operator_t

contains

  subroutine init_particles_on_demand(self, input)
    class(particles_on_demand_collision_operator_t), intent(inout) :: self
    class(input_t), intent(in) :: input

    self%name = "Particles on Demand"
    ! self%tau = input%relaxation_time

  end subroutine init_particles_on_demand

  subroutine particles_on_demand_collide(self)
    class(particles_on_demand_collision_operator_t), intent(in) :: self
  end subroutine

  ! elemental real(rk) function particles_on_demand_collide(self, pre_collision_populations, density) result(post_collision_populations)

  !   ! Input
  !   class(particles_on_demand_collision_operator_t), intent(in) :: self
  !   real(rk), intent(in) :: pre_collision_populations
  !   real(rk), intent(in) :: density

  !   associate(f_lambda => pre_collision_populations, &
  !             density => rho, &
  !             self%relaxation_parameter => beta, &
  !             self%weights => Wi
  !             f => post_collision_populations)

  !     f = f_lambda + 2*beta * (rho * Wi - f_lambda)
  !   end associate

  ! end function particles_on_demand_collide

end module mod_particles_on_demand_collsion_operator
