    module sub2d_element_module
        use parameter_module
        use tri_element_module
        use quad_element_module
        use coh2d_element_module
    
        implicit none
        private
    
        type,public :: sub2d_element
            private ! encapsulate components of this type
            
            character(len=namelength)       :: eltype       ! can be all types of 2D elements
            integer                         :: matkey       ! material index in glb material array
            real(kind=dp)                   :: theta        ! fibre orientation (lamina)
            logical                         :: plstrain     ! true for plane strain analysis
            integer,allocatable             :: subcnc(:)    ! sub_elem connec to parent elem nodes, size w.r.t eltype
            
            type(tri_element), allocatable  :: tri(:)       ! tri sub elements
            type(quad_element),allocatable  :: quad(:)      ! quad sub elements
            type(coh2d_element),allocatable :: coh2d(:)     ! 2D coh. sub elements
            
            real(kind=dp),allocatable       :: Tmatrix(:,:) ! interpolation matrix
        end type
    
        interface empty
            module procedure empty_sub2d_element
        end interface
      
        interface prepare
            module procedure prepare_sub2d_element
        end interface
        
        interface integrate
            module procedure integrate_sub2d_element
        end interface
        
        interface extract
            module procedure extract_sub2d_element
        end interface
    
    


        public :: empty,prepare,integrate,extract
        
        
        
        
        contains
        
        
        
        
        
        
    
    end sub2d_element_module