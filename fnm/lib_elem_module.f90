    !***************************************!
    !   the global library of elements      !
    !   used in the analysis;               !
    !                                       !
    !   this module is generated by the     !
    !   preprocessing programme             !
    !***************************************!
 
    include 'lib_node_module.f90'
    include 'lib_mat_module.f90'
    include 'integration_point_module.f90'
    include 'toolkit_module.f90'
    include 'tri_element_module.f90'
    !include 'quad_element_module.f90'
    !include 'wedge_element_module.f90'
    !include 'brick_element_module.f90'
    ! ... and other future element modules ...
   
    module lib_elem_module
    use parameter_module
    use tri_element_module
    ! ... and other future element modules ...
    
    implicit none
    save
    
    type(tri_element) :: lib_tri(2)
    
    contains
    
    subroutine initialize_lib_elem
    
        integer :: i=0
    
        do i=1, size(lib_tri)
            call empty(lib_tri(i))
        end do
        
        call prepare(lib_tri(1),key=1,connec=[1,2,3],matkey=1)
        call prepare(lib_tri(2),key=2,connec=[2,4,3],matkey=1)
    
    end subroutine initialize_lib_elem
      
    
    end module lib_elem_module