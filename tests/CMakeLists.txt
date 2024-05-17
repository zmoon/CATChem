add_executable(test_main test_main.F90)
target_link_libraries(test_main PRIVATE CATChem_core)
set_target_properties(test_main PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)

enable_testing()

add_test(
  NAME test_main
  COMMAND test_main
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
