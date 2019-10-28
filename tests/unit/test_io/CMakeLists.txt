
foreach(exec input)
add_executable(test_${exec} test_${exec}.f90)
target_link_libraries(test_${exec} lbm)
add_test(NAME ${exec}
         COMMAND ${Coarray_EXECUTABLE}
                 ${Coarray_NUMPROC_FLAG}
                 1
                 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test_${exec})
endforeach()

# add_pfunit_ctest(test_d2q9_lattice
#                  TEST_SOURCES
#                  test_d2q9.pf
#                  LINK_LIBRARIES
#                  lbm
#                  )