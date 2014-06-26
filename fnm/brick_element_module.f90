    module brick_element_module
    use paramter_module
    use xgeometry_module
    use material_module
    
    implicit none
    private
    
    integer,parameter :: ndim=3, nst=6, nnd=4, nintg=8, ndof=12 ! constants for this element type
    
    type, public :: brick_intg_point
        real(kind=dp) :: xi(ndim),x(ndim) ! natural and physical coordinates
        real(kind=dp) :: glb_stress(nst), lcl_stress(nst) ! stresses for output
        real(kind=dp) :: glb_strain(nst), lcl_strain(nst) ! strains for output
        real(kind=dp),allocatable :: sdv(:) ! sdvs for calculation and output
    end type :: brick_intg_point
    
    type, public :: brick_element 
        type(xbrick) :: connec ! node, edge, face connectivites to global databases and its status variable
        type(xnode) :: end_nodes(nnd) ! x, u, du, v, extra dof, ddof
        type(brick_intg_point) :: intg_points(nintg) ! x, xi, stress, strain, sdv
        real(kind=dp) :: k_matrix(ndof,ndof), f_vector(ndof) ! k matrix and f vector
        real(kind=dp) :: avg_glb_stress(nst), avg_lcl_stress(nst)
        real(kind=dp) :: avg_glb_strain(nst), avg_lcl_strain(nst)
    end type
    
    interface empty
        module procedure empty_brick_element
    end interface

    interface update
        module procedure update_brick_element
    end interface
    
    interface integrate
        module procedure integrate_brick_element
    end interface


    public :: empty,update,integrate
    
    
    contains
    
    
    
    
    
    end module brick_element_module