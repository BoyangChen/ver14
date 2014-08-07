    !***************************************! 
    !   the global library of edges         ! 
    !***************************************! 
                                              
    module lib_edge_module                    
    use parameter_module                      
                                              
    implicit none                             
    save                                      
                                              
    integer,allocatable :: lib_edge(:)        
                                              
    contains                                  
                                              
    subroutine initialize_lib_edge()          
                                              
        integer :: nedge=0                    
        integer :: i=0                        
                                              
        nedge=8   
        allocate(lib_edge(nedge))   
        lib_edge=0                  
    end subroutine initialize_lib_edge        
    end module lib_edge_module                
