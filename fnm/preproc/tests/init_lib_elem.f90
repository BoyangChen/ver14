    subroutine initialize_lib_elem()                                      
                                                                          
        integer ::  nelem=0, ntri=0, nquad=0, nwedge=0, nbrick=0 &        
        &          ,ncoh2d=0, ncoh3d6=0, ncoh3d8=0, nsub2d=0, nsub3d=0 &  
        &          ,nxquad=0, nxbrick=0                                   
        integer :: i=0                                                    
        nelem=3            
        allocate(lib_elem(nelem))       
        nxbrick=2     
        allocate(lib_xbrick(nxbrick))   
        call prepare(lib_xbrick(1),key=1, & 
& nodecnc=[1,2,4,3,5,6,8,7,17, 18,19, 20,21, 22,23, 24,25, 26,27, 28,29, 30,31, 32], & 
& edgecnc=[1,2,3,4,5,6,7,8], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1),elname="xbrick",eltype="xbrick",typekey=1) 

        call prepare(lib_xbrick(2),key=3, & 
& nodecnc=[9,10,12,11,13,14,16,15,33, 34,35, 36,37, 38,39, 40,41, 42,43, 44,45, 46,47, 48], & 
& edgecnc=[9,10,11,12,13,14,15,16], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(3),elname="xbrick",eltype="xbrick",typekey=2) 

        ncoh3d8=1     
        allocate(lib_coh3d8(ncoh3d8))   
        call prepare(lib_coh3d8(1),key=2, & 
& connec=[5,6,8,7,9,10,12,11], & 
& matkey=1 ) 
        call update(lib_elem(2),elname="coh3d8",eltype="coh3d8",typekey=1) 

    end subroutine initialize_lib_elem        
