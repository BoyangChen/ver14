    include 'parameter_module.f90'
    include 'lib_elem_module.f90'

    program test_tri
        use parameter_module
        use lib_elem_module

        implicit none

        call initialize_lib_elem

    end program test_tri
