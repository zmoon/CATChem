set(_srcs
    charpak_mod.F90
    constants.F90
    diagnostic_mod.F90
    error_mod.F90
    init_mod.F90
    precision_mod.F90
    qfyaml_mod.F90
    species_mod.F90
    test_main.F90)

add_library(CATChem_core STATIC ${_srcs})
