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
                                              
    contains                                  
                                              
    subroutine initialize_lib_node()          
                                              
        integer :: nnode=0                    
        integer :: i=0                        
        nnode=24   
        allocate(lib_node(nnode))   
        call update(lib_node(1),x=[-1.0_dp,-1.0_dp,0.0_dp],u=[zero,zero,zero])
        call update(lib_node(2),x=[1.0_dp,-1.0_dp,0.0_dp],u=[zero,zero,zero])
        call update(lib_node(3),x=[-1.0_dp,1.0_dp,0.0_dp],u=[zero,zero,zero])
        call update(lib_node(4),x=[1.0_dp,1.0_dp,0.0_dp],u=[zero,zero,zero])
        call update(lib_node(5),x=[-1.0_dp,-1.0_dp,0.25_dp],u=[zero,zero,zero])
        call update(lib_node(6),x=[1.0_dp,-1.0_dp,0.25_dp],u=[zero,zero,zero])
        call update(lib_node(7),x=[-1.0_dp,1.0_dp,0.25_dp],u=[zero,zero,zero])
        call update(lib_node(8),x=[1.0_dp,1.0_dp,0.25_dp],u=[zero,zero,zero])
        call update(lib_node(9),x=[0.0_dp,0.0_dp,0.0_dp],u=[zero,zero,zero])
        call update(lib_node(10),x=[0.0_dp,0.0_dp,0.0_dp],u=[zero,zero,zero])
        call update(lib_node(11),x=[0.0_dp,0.0_dp,0.0_dp],u=[zero,zero,zero])
        call update(lib_node(12),x=[0.0_dp,0.0_dp,0.0_dp],u=[zero,zero,zero])
        call update(lib_node(13),x=[0.0_dp,0.0_dp,0.0_dp],u=[zero,zero,zero])
        call update(lib_node(14),x=[0.0_dp,0.0_dp,0.0_dp],u=[zero,zero,zero])
        call update(lib_node(15),x=[0.0_dp,0.0_dp,0.0_dp],u=[zero,zero,zero])
        call update(lib_node(16),x=[0.0_dp,0.0_dp,0.0_dp],u=[zero,zero,zero])
        call update(lib_node(17),x=[0.0_dp,0.0_dp,0.0_dp],u=[zero,zero,zero])
        call update(lib_node(18),x=[0.0_dp,0.0_dp,0.0_dp],u=[zero,zero,zero])
        call update(lib_node(19),x=[0.0_dp,0.0_dp,0.0_dp],u=[zero,zero,zero])
        call update(lib_node(20),x=[0.0_dp,0.0_dp,0.0_dp],u=[zero,zero,zero])
        call update(lib_node(21),x=[0.0_dp,0.0_dp,0.0_dp],u=[zero,zero,zero])
        call update(lib_node(22),x=[0.0_dp,0.0_dp,0.0_dp],u=[zero,zero,zero])
        call update(lib_node(23),x=[0.0_dp,0.0_dp,0.0_dp],u=[zero,zero,zero])
        call update(lib_node(24),x=[0.0_dp,0.0_dp,0.0_dp],u=[zero,zero,zero])
    end subroutine initialize_lib_node        
    end module lib_node_module                
