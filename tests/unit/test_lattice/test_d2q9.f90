module lattice_mod
  use mod_d2q9_type, only: d2q9_lattice_t
  use mod_kinds, only: ik, rk
  implicit none

  class(d2q9_lattice_t), allocatable :: lattice

contains

  subroutine initialize()
    allocate(d2q9_lattice_t :: lattice)
    call lattice%initialize()
  end subroutine initialize

  subroutine test_basic()
    !< Test basic settings like weights, dimension, etc.

    if(lattice%n_weights /= 9) then
      if(this_image() == 1) write(*, '(a,i0)') "Error: lattice%n_weights should be 9, but is: ", lattice%n_weights
      error stop
    endif

    if(lattice%dimension /= 2) then
      if(this_image() == 1) write(*, '(a,i0)') "Error: lattice%dimension should be 2, but is: ", lattice%dimension
      error stop
    endif
  end subroutine test_basic

  subroutine test_neighbors()
    !< Test the lattice neighbor values, so that the proper directional velocities are used in streaming

    integer :: i, j

    do i = 1, lattice%n_weights - 1
      associate(neighbor=>lattice%neighbor_indices(i))
        associate(node=>int(lattice%v_hat(i, :)), &
                  other_node=>int(lattice%v_hat(neighbor, :)))

          do j = 1, 2
            if(node(j) /= 0 .or. other_node(j) /= 0) then
              if(node(j) /= -other_node(j)) then
                if(this_image() == 1) then
                  write(*, *) "Error: lattice%neighbor_indices is wrong: [", node, "] -> neighbor: [", other_node, "]"
                end if
                error stop
              end if
            end if
          end do

        end associate
      end associate
    end do

  end subroutine test_neighbors

end module lattice_mod

program test_lattices

  use lattice_mod
  implicit none

  call initialize()
  call test_basic()
  call test_neighbors()

end program test_lattices
