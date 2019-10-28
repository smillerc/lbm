add_library(lbm
            lib/mod_kinds.f90
            lib/collision_operators/collision_operator_factory.f90
            lib/collision_operators/bgk_collision.f90
            lib/collision_operators/entropic_collision.f90
            lib/collision_operators/collision_operator.f90
            lib/collision_operators/particles_on_demand_collision.f90
            lib/lattices/d2q9_type.f90
            lib/lattices/lattice_type.f90
            lib/lattices/lattice_factory.f90
            lib/io/mod_io_factory.f90
            lib/io/mod_input.f90
            lib/io/mod_io.f90
            lib/io/mod_xdmf.f90
            lib/io/mod_hdf5.f90
            lib/io/ini_parser/string_conv_mod.f90
            lib/io/ini_parser/cfgio_mod.f90
            lib/simulation/simulation_type.f90
            lib/simulation/sim_2d_type.f90
            lib/simulation/sim_factory.f90
            lib/streaming_operators/streaming_operator_factory.f90
            lib/fluids/fluid_factory.f90
            lib/fluids/fluid_2d_type.f90
            lib/fluids/2d_fluid.f90
            lib/fluids/fluid_type.f90
            lib/eos/eos_factory.f90
            )



include_directories(${HDF5_INCLUDE_DIRS})
target_include_directories(lbm PUBLIC ${HDF5_INCLUDE_DIRS})

target_compile_options(lbm PUBLIC ${Coarray_COMPILE_OPTIONS})
target_link_libraries(lbm
                      PUBLIC 
                      ${Coarray_LIBRARIES}
                             functional
                             ${HDF5_LIBRARIES}
                             ${HDF5_HL_LIBRARIES})

add_executable(lbm.x lbm.f90)

# set_target_properties (lbm PROPERTIES
#   Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

# target_compile_options(lbm.x PUBLIC ${Coarray_COMPILE_OPTIONS})
target_link_libraries(lbm.x
                      lbm
                      functional
                      ${HDF5_LIBRARIES}
                      ${HDF5_HL_LIBRARIES}
                      ${Coarray_LIBRARIES}
                      )
