    !***************************************!                             
    !   the global library of elements      !                             
    !***************************************!                             
    include "elements/tri_element_module.f90"                             
    include "elements/quad_element_module.f90"                            
    include "elements/coh2d_element_module.f90"                           
    include "elements/sub2d_element_module.f90"                           
    include "elements/xquad_element_module.f90"                           
    include "elements/wedge_element_module.f90"                           
    include "elements/brick_element_module.f90"                           
    include "elements/coh3d6_element_module.f90"                          
    include "elements/coh3d8_element_module.f90"                          
    include "elements/sub3d_element_module.f90"                           
    include "elements/xbrick_element_module.f90"                          
    include "elements/element_module.f90"                                 
                                                                          
                                                                          
    module lib_elem_module                                                
    use parameter_module                                                  
    use tri_element_module                                                
    use quad_element_module                                               
    use coh2d_element_module                                              
    use sub2d_element_module                                              
    use xquad_element_module                                              
    use wedge_element_module                                              
    use brick_element_module                                              
    use coh3d6_element_module                                             
    use coh3d8_element_module                                             
    use sub3d_element_module                                              
    use xbrick_element_module                                             
    use element_module                                                    
                                                                          
    implicit none                                                         
    save                                                                  
                                                                          
    type(element),          allocatable :: lib_elem(:)                    
    type(tri_element),      allocatable :: lib_tri(:)                     
    type(quad_element),     allocatable :: lib_quad(:)                    
    type(coh2d_element),    allocatable :: lib_coh2d(:)                   
    type(xquad_element),    allocatable :: lib_xquad(:)                   
    type(wedge_element),    allocatable :: lib_wedge(:)                   
    type(brick_element),    allocatable :: lib_brick(:)                   
    type(coh3d6_element),   allocatable :: lib_coh3d6(:)                  
    type(coh3d8_element),   allocatable :: lib_coh3d8(:)                  
    type(xbrick_element),   allocatable :: lib_xbrick(:)                  
                                                                          
    contains                                                              
                                                                          
    subroutine initialize_lib_elem                                        
                                                                          
        integer ::  nelem=0, ntri=0, nquad=0, nwedge=0, nbrick=0 &        
        &          ,ncoh2d=0, ncoh3d6=0, ncoh3d8=0, nsub2d=0, nsub3d=0 &  
        &          ,nxquad=0, nxbrick=0                                   
        integer :: i=0                                                    
        nelem=1            
        allocate(lib_elem(nelem))       
        nxbrick=1     
        allocate(lib_xbrick(nxbrick))   
        call prepare(lib_xbrick(1),key=1, & 
& nodecnc=[1,2,4,3,5,6,8,7,9, 10,11, 12,13, 14,15, 16,17, 18,19, 20,21, 22,23, 24], & 
& edgecnc=[1,2,3,4,5,6,7,8], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1),elname="xbrick",eltype="xbrick",typekey=1) 

    end subroutine initialize_lib_elem        
    end module lib_elem_module                
