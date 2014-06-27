    module tri_element_module
    use paramter_module
    use xgeometry_module
    use material_module
    
    implicit none
    private
    
    integer,parameter :: ndim=2, nst=3, nnd=3, nintg=1, ndof=ndim*nnd ! constants for type tri_element
    
    type, public :: tri_intg_point
        real(kind=dp) :: xi(ndim),x(ndim) ! natural and physical coordinates
        real(kind=dp) :: glb_stress(nst), lcl_stress(nst) ! stresses for output
        real(kind=dp) :: glb_strain(nst), lcl_strain(nst) ! strains for output
        real(kind=dp),allocatable :: sdv(:) ! sdvs for calculation and output
    end type :: tri_intg_point
    
    type, public :: tri_element 
        integer :: connec(nnd) ! node indices in their global arrays
        type(tri_intg_point) :: intg_point(nintg) ! x, xi, stress, strain, sdv
        real(kind=dp) :: K_matrix(ndof,ndof), F_vector(ndof) ! k matrix and f vector
        real(kind=dp) :: avg_glb_stress(nst), avg_lcl_stress(nst)
        real(kind=dp) :: avg_glb_strain(nst), avg_lcl_strain(nst)
        real(kind=dp),allocatable :: avg_sdv(:)
    end type
    
    interface empty
        module procedure empty_tri_element
    end interface

    interface update
        module procedure update_tri_element
    end interface
    
    interface integrate
        module procedure integrate_tri_element
    end interface


    public :: empty,update,integrate
    
    
    contains
    
    
    
    
    
    end module tri_element_module