module xlam_element_module


    implicit none
    private
    

    type, public :: xlam_element
    
        type(xbrick_element),allocatable :: plyblk(:)
        type(coh3d8_element),allocatable :: interf(:)
    
    end type xlam_element
    
    
    

end module xlam_element_module