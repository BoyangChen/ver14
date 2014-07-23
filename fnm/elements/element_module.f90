 
    module element_module
    use parameter_module
    
    implicit none
  
    
    
    type :: element                                 ! global element array is on this type
        
        private
        
        character(len=elnamelength) :: elname=''    ! user-given name from input
        character(len=eltypelength) :: eltype=''    ! defined element types
        integer                     :: elkey=0      ! index in the respective array of the associated element type
        
    end type
    
    
    

    interface empty
        module procedure empty_element
    end interface
    
    interface update
        module procedure update_element
    end interface
    
    interface extract
        module procedure extract_element
    end interface
    
    private :: empty_element, update_element, extract_element
    
    
    
    contains
    
    
    
    
    ! this subroutine is used in the preprocessing to format the 
    ! global element library lib_mat
    subroutine empty_element(elem)
        type(element),intent(out):: elem
        
        elem%elname=''
        elem%eltype=''
        elem%elkey=0
    
    end subroutine empty_element
    
    
    ! this subroutine is used in the preprocessing to fill in the
    ! element information in the global element library lib_mat
    subroutine update_element(elem,elname,eltype,elkey)
        type(element),                          intent(inout)   :: elem
        character(*),               optional,   intent(in)      :: elname 
        character(*),               optional,   intent(in)      :: eltype 
        integer,                    optional,   intent(in)      :: elkey
        
        if(present(elname)) elem%elname=elname
        if(present(eltype)) elem%eltype=eltype
        if(present(elkey))  elem%elkey=elkey
    
    end subroutine update_element
    
    
    ! this subroutine is used anywhere to extract element information
    subroutine extract_element(elem,elname,eltype,elkey)
        type(element),                          intent(in)  :: elem
        character(len=elnamelength),optional,   intent(out) :: elname 
        character(len=eltypelength),optional,   intent(out) :: eltype 
        integer,                    optional,   intent(out) :: elkey
        
        if(present(elname)) elname=elem%elname
        if(present(eltype)) eltype=elem%eltype
        if(present(elkey)) elkey=elem%elkey
    
    end subroutine extract_element
    
    
    end module element_module