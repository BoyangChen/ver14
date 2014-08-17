    module initialize_lib_module
    use parameter_module
    use lib_mat_module
    use lib_node_module
    use lib_edge_module
    use lib_elem_module
    
    contains
    
include 'libraries/init_lib_node.f90'
    
include 'libraries/init_lib_edge.f90'
    
include 'libraries/init_lib_elem.f90'
    
include 'libraries/init_lib_mat.f90'
    
    end module initialize_lib_module