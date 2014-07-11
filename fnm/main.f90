    include 'globals/parameter_module.f90'
    include 'globals/glb_clock_module.f90'
    include 'libraries/lib_node_module.f90'
    include 'libraries/lib_mat_module.f90'
    include 'libraries/lib_elem_module.f90'
    include 'outputs/output_module.f90'

    program test_main
        use parameter_module
        use glb_clock_module
        use lib_mat_module
        use lib_node_module
        use lib_elem_module
        use output_module

        implicit none

        type :: nodeset

            character(len=setnamelength) :: setname
            integer,allocatable :: setnode(:)

        end type


        integer, parameter :: ndim=3 ! dimension of problem

        ! K, F and connectivity of each element, extracted from glb lib
        real(kind=dp),allocatable :: Ki(:,:), Fi(:), strsi(:), strni(:)
        integer, allocatable :: conneci(:)

        ! K, F and U of the global system, assembled from glb lib
        real(kind=dp),allocatable :: K(:,:),U(:),F(:)

        ! boundary node sets
        type(nodeset),allocatable :: ndset(:)

        ! output directory name
        character(len=dirlength)  :: workdir, outdir

        integer :: i,jl,ml,nl,jr,mr,nr,nnode,ndof,nelem
        integer :: ntri,nquad,nwedge,nbrick,ncoh2d,ncoh3d6,ncoh3d8
        integer :: kinc, ninc, nndset
        real(dp):: ux, uy, uz, vx, vy, vz




        ! initialize local variables

        outdir=''
        i=0
        jl=0; ml=0; nl=0
        jr=0; mr=0; nr=0
        nnode=0; ndof=0
        nelem=0; ntri=0; nquad=0; nwedge=0; nbrick=0
        ncoh2d=0; ncoh3d6=0; ncoh3d8=0
        nndset=0; kinc=0; ninc=0
        ux=zero; uy=zero; uz=zero
        vx=zero; vy=zero; vz=zero


        ! initialize global clock and libraries

        call initialize_glb_clock
        call initialize_lib_node
        call initialize_lib_elem
        call initialize_lib_mat


        ! initialize node sets

        nndset=2
        allocate(ndset(nndset))

!        ndset(1)=nodeset(setname='bottom',setnode=[1,2])
!        ndset(2)=nodeset(setname='top',setnode=[3,4])

        ndset(1)=nodeset(setname='bottom',setnode=[1,2,3,4,5,6])
        ndset(2)=nodeset(setname='top',setnode=[7,8,9,10,11,12])


        ! obtain the current working directory and specify output directory
        call getcwd(workdir)
        outdir=trim(workdir)//'/outputs/'


        ! set loading rates and increments
        vx=-0.01_dp
        vy=0.01_dp
        vz=0.01_dp
        ninc=10


        ! -----------------------------------------------!
        !   perform step by step loading on boundaries   !
        ! -----------------------------------------------!

        do kinc=1,ninc


            ! update global clock
            call update_glb_clock(kstep=1,kinc=kinc)

            ux=vx*kinc
            uy=vy*kinc
            uz=vz*kinc

            ! input here some test disp. loading
            do i=1,size(ndset(2)%setnode)
                if(ndim==2) then
                    call update(lib_node(ndset(2)%setnode(i)),u=[ux,uy])
                else if(ndim==3) then
                    call update(lib_node(ndset(2)%setnode(i)),u=[ux,uy,uz])
                end if
            end do



            ! integration and assembly
            if(allocated(lib_tri)) then
                ntri=size(lib_tri)
                do i=1,ntri
                    call integrate(lib_tri(i),Ki,Fi)
                end do
            end if

            if(allocated(lib_quad)) then
                nquad=size(lib_quad)
                do i=1,nquad
                    call integrate(lib_quad(i),Ki,Fi)
                end do
            end if

            if(allocated(lib_wedge)) then
                nwedge=size(lib_wedge)
                do i=1,nwedge
                    call integrate(lib_wedge(i),Ki,Fi)
                end do
            end if

            if(allocated(lib_brick)) then
                nbrick=size(lib_brick)
                do i=1,nbrick
                    call integrate(lib_brick(i),Ki,Fi)
                end do
            end if

            if(allocated(lib_coh2d)) then
                ncoh2d=size(lib_coh2d)
                do i=1,ncoh2d
                    call integrate(lib_coh2d(i),Ki,Fi)
                end do
            end if

            if(allocated(lib_coh3d6)) then
                ncoh3d6=size(lib_coh3d6)
                do i=1,ncoh3d6
                    call integrate(lib_coh3d6(i),Ki,Fi)
                end do
            end if

            if(allocated(lib_coh3d8)) then
                ncoh3d8=size(lib_coh3d8)
                do i=1,ncoh3d8
                    call integrate(lib_coh3d8(i),Ki,Fi)
                end do
            end if


            !print*,outdir
            call output(1,kinc,outdir)

        end do

    end program test_main
