    include 'parameter_module.f90'
    include 'lib_node_module.f90'
    include 'lib_mat_module.f90'
    include 'lib_elem_module.f90'

    program test_tri
        use parameter_module
        use lib_mat_module
        use lib_node_module
        use lib_elem_module

        implicit none

        real(kind=dp),allocatable :: K1(:,:), F1(:)
        real(kind=dp),allocatable :: K2(:,:), F2(:)
        real(kind=dp)             :: K(8,8),u(8),F(8)
        integer, allocatable :: connec1(:), connec2(:)
        integer :: i,j,n,ndim

        K=zero; u=zero; F=zero
        i=0; j=0; n=0; ndim=2

        call initialize_lib_node
        call initialize_lib_elem
        call initialize_lib_mat

        call extract(lib_tri(1),connec=connec1)
        call extract(lib_tri(2),connec=connec2)

        call integrate(lib_tri(1),K1,F1)
        call integrate(lib_tri(2),K2,F2)



        u(6)=0.1_dp
        u(8)=0.1_dp

        F=matmul(K,u)
        print*,F

    end program test_tri
