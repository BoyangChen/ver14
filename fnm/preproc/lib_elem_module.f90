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
        allocate(lib_xbrick(nxbrick))   
        call prepare(lib_xbrick(1),key=1, & 
& nodecnc=[1,2,6,5,17,18,22,21,33, 34,35, 36,37, 38,39, 40,41, 42,43, 44,45, 46,47, 48], & 
& edgecnc=[1,2,3,4,5,6,7,8], & 
& bulkmat=0, cohmat=0 ) 
        call update(lib_elem(1),elname="xbrick",eltype="xbrick",typekey=1) 

        call prepare(lib_xbrick(2),key=2, & 
& nodecnc=[2,3,7,6,18,19,23,22,49, 50,51, 52,53, 54,36, 35,55, 56,57, 58,59, 60,44, 43], & 
& edgecnc=[9,10,11,2,12,13,14,6], & 
& bulkmat=0, cohmat=0 ) 
        call update(lib_elem(2),elname="xbrick",eltype="xbrick",typekey=2) 

        call prepare(lib_xbrick(3),key=3, & 
& nodecnc=[3,4,8,7,19,20,24,23,61, 62,63, 64,65, 66,52, 51,67, 68,69, 70,71, 72,58, 57], & 
& edgecnc=[15,16,17,10,18,19,20,13], & 
& bulkmat=0, cohmat=0 ) 
        call update(lib_elem(3),elname="xbrick",eltype="xbrick",typekey=3) 

        call prepare(lib_xbrick(4),key=4, & 
& nodecnc=[5,6,10,9,21,22,26,25,38, 37,73, 74,75, 76,77, 78,46, 45,79, 80,81, 82,83, 84], & 
& edgecnc=[3,21,22,23,7,24,25,26], & 
& bulkmat=0, cohmat=0 ) 
        call update(lib_elem(4),elname="xbrick",eltype="xbrick",typekey=4) 

        call prepare(lib_xbrick(5),key=5, & 
& nodecnc=[6,7,11,10,22,23,27,26,54, 53,85, 86,87, 88,74, 73,60, 59,89, 90,91, 92,80, 79], & 
& edgecnc=[11,27,28,21,14,29,30,24], & 
& bulkmat=0, cohmat=0 ) 
        call update(lib_elem(5),elname="xbrick",eltype="xbrick",typekey=5) 

        call prepare(lib_xbrick(6),key=6, & 
& nodecnc=[7,8,12,11,23,24,28,27,66, 65,93, 94,95, 96,86, 85,72, 71,97, 98,99, 100,90, 89], & 
& edgecnc=[17,31,32,27,20,33,34,29], & 
& bulkmat=0, cohmat=0 ) 
        call update(lib_elem(6),elname="xbrick",eltype="xbrick",typekey=6) 

        call prepare(lib_xbrick(7),key=7, & 
& nodecnc=[9,10,14,13,25,26,30,29,76, 75,101, 102,103, 104,105, 106,82, 81,107, 108,109, 110,111, 112], & 
& edgecnc=[22,35,36,37,25,38,39,40], & 
& bulkmat=0, cohmat=0 ) 
        call update(lib_elem(7),elname="xbrick",eltype="xbrick",typekey=7) 

        call prepare(lib_xbrick(8),key=8, & 
& nodecnc=[10,11,15,14,26,27,31,30,88, 87,113, 114,115, 116,102, 101,92, 91,117, 118,119, 120,108, 107], & 
& edgecnc=[28,41,42,35,30,43,44,38], & 
& bulkmat=0, cohmat=0 ) 
        call update(lib_elem(8),elname="xbrick",eltype="xbrick",typekey=8) 

        call prepare(lib_xbrick(9),key=9, & 
& nodecnc=[11,12,16,15,27,28,32,31,96, 95,121, 122,123, 124,114, 113,100, 99,125, 126,127, 128,118, 117], & 
& edgecnc=[32,45,46,41,34,47,48,43], & 
& bulkmat=0, cohmat=0 ) 
        call update(lib_elem(9),elname="xbrick",eltype="xbrick",typekey=9) 

        ncoh3d8=9     
        allocate(lib_coh3d8(ncoh3d8))   
        call prepare(lib_coh3d8(1),key=10, & 
& connec=[1,2,6,5,17,18,22,21], & 
& matkey=0 ) 
        call update(lib_elem(10),elname="coh3d8",eltype="coh3d8",typekey=1) 

        call prepare(lib_coh3d8(2),key=11, & 
& connec=[2,3,7,6,18,19,23,22], & 
& matkey=0 ) 
        call update(lib_elem(11),elname="coh3d8",eltype="coh3d8",typekey=2) 

        call prepare(lib_coh3d8(3),key=12, & 
& connec=[3,4,8,7,19,20,24,23], & 
& matkey=0 ) 
        call update(lib_elem(12),elname="coh3d8",eltype="coh3d8",typekey=3) 

        call prepare(lib_coh3d8(4),key=13, & 
& connec=[5,6,10,9,21,22,26,25], & 
& matkey=0 ) 
        call update(lib_elem(13),elname="coh3d8",eltype="coh3d8",typekey=4) 

        call prepare(lib_coh3d8(5),key=14, & 
& connec=[6,7,11,10,22,23,27,26], & 
& matkey=0 ) 
        call update(lib_elem(14),elname="coh3d8",eltype="coh3d8",typekey=5) 

        call prepare(lib_coh3d8(6),key=15, & 
& connec=[7,8,12,11,23,24,28,27], & 
& matkey=0 ) 
        call update(lib_elem(15),elname="coh3d8",eltype="coh3d8",typekey=6) 

        call prepare(lib_coh3d8(7),key=16, & 
& connec=[9,10,14,13,25,26,30,29], & 
& matkey=0 ) 
        call update(lib_elem(16),elname="coh3d8",eltype="coh3d8",typekey=7) 

        call prepare(lib_coh3d8(8),key=17, & 
& connec=[10,11,15,14,26,27,31,30], & 
& matkey=0 ) 
        call update(lib_elem(17),elname="coh3d8",eltype="coh3d8",typekey=8) 

        call prepare(lib_coh3d8(9),key=18, & 
& connec=[11,12,16,15,27,28,32,31], & 
& matkey=0 ) 
        call update(lib_elem(18),elname="coh3d8",eltype="coh3d8",typekey=9) 

        nelem=18            
        allocate(lib_elem(nelem))       
    end subroutine initialize_lib_elem        
    end module lib_elem_module                
