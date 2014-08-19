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
    include "elements/xlam_element_module.f90"
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
    use xlam_element_module
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
    type(xlam_element),     allocatable :: lib_xlam(:)

    contains
    
    subroutine empty_lib_elem()
    
    if(allocated(lib_elem)) deallocate(lib_elem)                     
    if(allocated(lib_tri)) deallocate(lib_tri)                    
    if(allocated(lib_quad)) deallocate(lib_quad)                    
    if(allocated(lib_coh2d)) deallocate(lib_coh2d)                
    if(allocated(lib_xquad)) deallocate(lib_xquad)                   
    if(allocated(lib_wedge)) deallocate(lib_wedge)                 
    if(allocated(lib_brick)) deallocate(lib_brick)                  
    if(allocated(lib_coh3d6)) deallocate(lib_coh3d6)                 
    if(allocated(lib_coh3d8)) deallocate(lib_coh3d8)                  
    if(allocated(lib_xbrick)) deallocate(lib_xbrick)
    if(allocated(lib_xlam)) deallocate(lib_xlam)
    
    end subroutine empty_lib_elem
                                                        
    end module lib_elem_module                
