add_library(lbm
    #         lib/collision_operators/mod_collision_operator.f90
    #         lib/fluids/mod_base_fluid.f90
    #         lib/fluids/mod_charged_fluid.f90
    #         lib/fluids/mod_electron_fluid.f90
    #         lib/fields/mod_em_field.f90
    #         lib/fields/mod_field_abstract.f90
    #         lib/fluids/mod_ion_fluid.f90
    #    #      lib/fields/mod_magnetic_field.f90
    #         lib/fluids/mod_neutral_fluid.f90
    #       #   lib/fluids/mod_neutral_fluid_1d.f90
    #         lib/fluids/mod_neutral_fluid_2d.f90
    #       #   lib/fluids/mod_neutral_fluid_3d.f90
    #         lib/grid/mod_grid.f90
    #         lib/grid/mod_grid_factory.f90
    #         lib/grid/mod_grid_two_fluid.f90
    #         lib/grid/mod_grid_single_fluid_1d.f90
    #         lib/grid/mod_grid_single_fluid_2d.f90
    #         lib/grid/mod_grid_single_fluid_3d.f90
            lib/io/ini_parser/cfgio_mod.f90
            lib/io/ini_parser/string_conv_mod.f90
            # lib/io/mod_io_factory.f90
            # lib/io/mod_io.f90
            # lib/io/mod_hdf5.f90
            lib/io/mod_input.f90
            # lib/io/mod_xdmf.f90
            # lib/lattice_types/mod_d1q3.f90
            # lib/lattice_types/mod_d2q9.f90
            # lib/lattice_types/mod_d3q15.f90
            # lib/lattice_types/mod_d3q19.f90
            # lib/lattice_types/mod_d3q19_em.f90
            # lib/lattice_types/mod_d3q27.f90
            # lib/lattice_types/mod_lattice_abstract.f90
            # lib/lattice_types/mod_lattice_factory.f90
            # lib/mod_arrays.f90
            # lib/mod_constants.f90
            # lib/mod_eos.f90
            lib/mod_kinds.f90
            # lib/parallel/mod_parallel.f90
            # lib/throw.f90
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
