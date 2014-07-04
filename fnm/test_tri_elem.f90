    include 'parameter_module.f90'
    include 'lib_node_module.f90'
    include 'lib_mat_module.f90'
    include 'lib_elem_module.f90'
    include 'output_module.f90'

    program test_tri
        use parameter_module
        use lib_mat_module
        use lib_node_module
        use integration_point_module
        use lib_elem_module
        use output_module

        implicit none

        type :: nodeset

            character(len=setnamelength) :: setname
            integer,allocatable :: setnode(:)

        end type


        integer, parameter :: ndim=2 ! dimension of problem

        ! K, F and connectivity of each element, extracted from glb lib
        real(kind=dp),allocatable :: Ki(:,:), Fi(:), strsi(:), strni(:)
        integer, allocatable :: conneci(:)
        type(integration_point),allocatable :: igpt(:)

        ! K, F and U of the global system, assembled from glb lib
        real(kind=dp),allocatable :: K(:,:),U(:),F(:)

        ! boundary node sets
        type(nodeset),allocatable :: ndset(:)

        ! output directory name
        character(len=dirlength)  :: outdir

        integer :: i,jl,ml,nl,jr,mr,nr,nnode,ndof,nelem,ntri,nndset

        ! initialize variables
        outdir=''
        i=0
        jl=0; ml=0; nl=0
        jr=0; mr=0; nr=0
        nnode=0; ndof=0
        nelem=0; ntri=0;nndset=0

        call initialize_lib_node
        call initialize_lib_elem
        call initialize_lib_mat

        ! obtain nnode value from glb libraries
        nnode=size(lib_node)

        ! total no. of dof
        ndof=ndim*nnode

        ! allocate K, F and U
        allocate(K(ndof,ndof),F(ndof),U(ndof))

        ! initialize K, F and U
        K=zero; F=zero; U=zero


        ! Apply boundary test loading
        nndset=2
        allocate(ndset(nndset))

        ndset(1)=nodeset(setname='bottom',setnode=[1,2])
        ndset(2)=nodeset(setname='top',setnode=[3,4])

        ! input here some test disp. loading
        ! uniform tension in y dir on top surf, u_y=0.1
        do i=1,size(ndset(2)%setnode)
            call update(lib_node(ndset(2)%setnode(i)),u=[0.1_dp,zero])
        end do


        ! integration and assembly
        if(allocated(lib_tri)) then
            ntri=size(lib_tri)
            do i=1,ntri
                call integrate(lib_tri(i),Ki,Fi)
!                call extract(lib_tri(i),connec=conneci)
!                do jr=1,3 ! 3 nodes
!                    mr=jr*ndim
!                    nr=conneci(jr)*ndim
!                    do jl=1,3
!                        ml=jl*ndim
!                        nl=conneci(jl)*ndim
!                        K(nl-1:nl,nr-1:nr)=K(nl-1:nl,nr-1:nr)+Ki(ml-1:ml,mr-1:mr)
!                    end do
!                end do
!                call extract(lib_tri(i),ig_point=igpt)
!                call extract(igpt(1),stress=strsi,strain=strni)
!                print*,strni,strsi
            end do
        end if

        ! obtain the current working directory
        call getcwd(outdir)
        !print*,outdir
        call output(1,outdir)

    end program test_tri
