    !***************************************! 
    !   the global library of nodes         ! 
    !***************************************! 
                                              
    include "globals/xnode_module.f90"        
                                              
    module lib_node_module                    
    use parameter_module                      
    use xnode_module                          
                                              
    implicit none                             
    save                                      
                                              
    type(xnode),allocatable :: lib_node(:)    
                                                
    end module lib_node_module                
