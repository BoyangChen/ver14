    module sub2d_element_module
        use parameter_module
        use tri_element_module
        use quad_element_module
        use coh2d_element_module
    
        implicit none
        private
    
        type,public :: sub2d_element
            private ! encapsulate components of this type
            
            character(len=namelength)       :: sub_type     ! all types of 2D elements
            integer,allocatable             :: sub_cnc(:)   ! sub_elem connec, allocate size according to sub_type
            real(kind=dp),allocatable       :: Tmatrix(:,:) ! interpolation matrix
            type(tri_element), allocatable  :: sub_tri(:)   ! tri sub elements
            type(quad_element),allocatable  :: sub_quad(:)  ! quad sub elements
            type(coh2d_element),allocatable :: sub_coh2d(:) ! 2D coh. sub elements
        end type
    
        interface empty
            module procedure empty_sub2d_element
        end interface
      
        interface update
            module procedure update_sub2d_element
        end interface
        
        public :: empty,update
        
        
        
        
        contains
        
        
        
        
        
        
    
    end sub2d_element_module