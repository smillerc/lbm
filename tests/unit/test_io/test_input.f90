module test_input_mod
  use mod_input, only: input_t

  implicit none

  character(:), allocatable :: input_filename
  class(input_t), allocatable :: input

contains

  subroutine test_init()
    allocate(input_t :: input)

    input_filename = 'simple.ini'
    call input%read(input_filename)

  end subroutine test_init

end module test_input_mod

program test_input
  use test_input_mod

  implicit none

  call test_init()

end program test_input
