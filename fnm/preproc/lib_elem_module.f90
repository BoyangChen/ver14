    !***************************************!                             
    !   the global library of elements      !                             
    !***************************************!                             
    include "elements/tri_element_module.f90"                             
    include "elements/quad_element_module.f90"                            
    include "elements/coh2d_element_module.f90"                           
    include "elements/sub2d_element_module.f90"                           
    include "elements/xquad_element_module.f90"                           
    include "elements/element_module.f90"                                 
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
        nxbrick=9     
        nelem=nxbrick                   
        allocate(lib_xbrick(nxbrick))   
        allocate(lib_elem(nelem))       
        call prepare(lib_xbrick(1),key=1, & 
& nodecnc=[1,2,6,5,17,18,22,21,32, 33,34, 35,36, 37,38, 39,40, 41,42, 43,44, 45,46, 47], & 
& edgecnc=[1,2,3,4,5,6,7,8], & 
& bulkmat=1, cohmat=2, theta=45._dp ) 
        call update(lib_elem(1),elname="xbrick",eltype="xbrick",elkey=1) 

        call prepare(lib_xbrick(2),key=2, & 
& nodecnc=[2,3,7,6,18,19,23,22,48, 49,50, 51,52, 53,35, 34,54, 55,56, 57,58, 59,43, 42], & 
& edgecnc=[9,10,11,2,12,13,14,6], & 
& bulkmat=1, cohmat=2, theta=45._dp ) 
        call update(lib_elem(2),elname="xbrick",eltype="xbrick",elkey=2) 

        call prepare(lib_xbrick(3),key=3, & 
& nodecnc=[3,4,8,7,19,20,24,23,60, 61,62, 63,64, 65,51, 50,66, 67,68, 69,70, 71,57, 56], & 
& edgecnc=[15,16,17,10,18,19,20,13], & 
& bulkmat=1, cohmat=2, theta=45._dp ) 
        call update(lib_elem(3),elname="xbrick",eltype="xbrick",elkey=3) 

        call prepare(lib_xbrick(4),key=4, & 
& nodecnc=[5,6,10,9,21,22,26,25,37, 36,72, 73,74, 75,76, 77,45, 44,78, 79,80, 81,82, 83], & 
& edgecnc=[3,21,22,23,7,24,25,26], & 
& bulkmat=1, cohmat=2, theta=45._dp ) 
        call update(lib_elem(4),elname="xbrick",eltype="xbrick",elkey=4) 

        call prepare(lib_xbrick(5),key=5, & 
& nodecnc=[6,7,11,10,22,23,27,26,53, 52,84, 85,86, 87,73, 72,59, 58,88, 89,90, 91,79, 78], & 
& edgecnc=[11,27,28,21,14,29,30,24], & 
& bulkmat=1, cohmat=2, theta=45._dp ) 
        call update(lib_elem(5),elname="xbrick",eltype="xbrick",elkey=5) 

        call prepare(lib_xbrick(6),key=6, & 
& nodecnc=[7,8,12,11,23,24,28,27,65, 64,92, 93,94, 95,85, 84,71, 70,96, 97,98, 99,89, 88], & 
& edgecnc=[17,31,32,27,20,33,34,29], & 
& bulkmat=1, cohmat=2, theta=45._dp ) 
        call update(lib_elem(6),elname="xbrick",eltype="xbrick",elkey=6) 

        call prepare(lib_xbrick(7),key=7, & 
& nodecnc=[9,10,14,13,25,26,30,29,75, 74,100, 101,102, 103,104, 105,81, 80,106, 107,108, 109,110, 111], & 
& edgecnc=[22,35,36,37,25,38,39,40], & 
& bulkmat=1, cohmat=2, theta=45._dp ) 
        call update(lib_elem(7),elname="xbrick",eltype="xbrick",elkey=7) 

        call prepare(lib_xbrick(8),key=8, & 
& nodecnc=[10,11,15,14,26,27,31,30,87, 86,112, 113,114, 115,101, 100,91, 90,116, 117,118, 119,107, 106], & 
& edgecnc=[28,41,42,35,30,43,44,38], & 
& bulkmat=1, cohmat=2, theta=45._dp ) 
        call update(lib_elem(8),elname="xbrick",eltype="xbrick",elkey=8) 

        call prepare(lib_xbrick(9),key=9, & 
& nodecnc=[11,12,16,15,27,28,32,31,95, 94,120, 121,122, 123,113, 112,99, 98,124, 125,126, 127,117, 116], & 
& edgecnc=[32,45,46,41,34,47,48,43], & 
& bulkmat=1, cohmat=2, theta=45._dp ) 
        call update(lib_elem(9),elname="xbrick",eltype="xbrick",elkey=9) 

    end subroutine initialize_lib_elem        
    end module lib_elem_module                
