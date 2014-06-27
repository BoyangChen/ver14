    module subelement_module
        use parameter_module
    
        implicit none
        private
    
        type,public :: subelement
            character(len=namelength) :: sub_type ! all types of elements
            integer,allocatable :: sub_cnc(:) ! allocate size according to sub_type
            real(kind=dp),allocatable :: Tmatrix(:,:) ! interpolation matrix
        end type
    
        interface empty
            module procedure empty_sub_element
        end interface
      
        interface update
            module procedure update_sub_element
        end interface
        
        public :: empty,update
        
        
        contains
        
        
        
    
    end subelement_module