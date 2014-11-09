    subroutine initialize_lib_bcd()           
                                              
        integer :: nbcdnodes=0                
        integer :: i=0                        
                                              
     nbcdnodes=16
     allocate(lib_bcdnodes(nbcdnodes))
     lib_bcdnodes=0
     lib_bcdnodes(1)=1
     lib_bcdnodes(2)=2
     lib_bcdnodes(3)=5
     lib_bcdnodes(4)=6
     lib_bcdnodes(5)=1
     lib_bcdnodes(6)=3
     lib_bcdnodes(7)=5
     lib_bcdnodes(8)=7
     lib_bcdnodes(9)=2
     lib_bcdnodes(10)=4
     lib_bcdnodes(11)=6
     lib_bcdnodes(12)=8
     lib_bcdnodes(13)=3
     lib_bcdnodes(14)=4
     lib_bcdnodes(15)=7
     lib_bcdnodes(16)=8
    end subroutine initialize_lib_bcd        
