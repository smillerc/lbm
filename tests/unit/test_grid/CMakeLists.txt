foreach(exec two_fluid_grid grid_factory)
add_executable(test_${exec} test_${exec}.f90)
target_link_libraries(test_${exec} plasma_lbm)
add_test(NAME ${exec}
         COMMAND ${Coarray_EXECUTABLE}
                 ${Coarray_NUMPROC_FLAG}
                 2
                 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test_${exec})
endforeach()