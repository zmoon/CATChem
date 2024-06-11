set(_srcs GOCART/Process_Library/GOCART_2G_Process.F90 GOCART/Process_Library/GOCART2G_MeiMod.F90)

set(_lib GOCART2G)

add_library(${_lib} ${_srcs} DEPENDENCIES NetCDF::NetCDF_Fortran)
target_link_libraries(${_lib} PUBLIC CATChem_core)
set_target_properties(${_lib} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR/include})
