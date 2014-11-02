    subroutine initialize_lib_elem()                                      
                                                                          
        integer ::  nelem=0, nxlam=0                                      
        integer :: i=0                                                    
        nelem=1            
        allocate(lib_elem(nelem))       
        nxlam=1     
        allocate(lib_xlam(nxlam))   
        call prepare(lib_xlam(1),key=1, & 
& bulkmat=1, cohmat=2, interfmat=3, & 
& nodecnc=[ & 
& 1,2,4,3,5,6,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,28,27,29,30 & 
& ,32,31,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48], & 
& edgecnc=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], & 
& layup=reshape([0,0.666666666667,90,0.333333333333],[2,2]) )
        call update(lib_elem(1),elname="xlam",eltype="xlam",typekey=1) 

    end subroutine initialize_lib_elem        
