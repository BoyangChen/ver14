    include 'parameter_module.f90'
    include 'xnode_module.f90'
    include 'material_module.f90'
    include 'integration_point_module.f90'

    program testdatahiding
    use parameter_module
    use xnode_module
    use material_module
    use integration_point_module

    implicit none

    !type(xnode) :: node1,node2
    !real(kind=dp) :: x(3)


    !call empty(node1)
    !call empty(node2)
    !call update(node1,x=[one,one,one])
    !node2=node1
    !call export(node2,x=x)

    !print*,x


!    type(isotropic_type) :: lib_iso(3)
!    integer :: i
!    logical :: mstat,sstat,tstat
!    i=0
!
!    do i=1,size(lib_iso)
!        call empty(lib_iso(i))
!    end do
!
!    call update(lib_iso(1),isotropic_modulus(E=one,nu=zero))
!
!    call export(lib_iso(1),modulus_active=mstat,strength_active=sstat,toughness_active=tstat)
!    !call export(lib_iso(1),mstat,sstat,tstat) ! this doesn't work
!
!    print*,mstat,sstat,tstat


    integer :: ndim=2, nst=3
    type(integration_point) :: igpnt
    real(kind=dp),allocatable :: x(:),rsdv(:)

    call update(igpnt,x=[zero,one],rsdv=[one,two])
    call export(igpnt,x=x,rsdv=rsdv)

    print*,x
    print*,rsdv

    call empty(igpnt)

    x=zero;rsdv=ten

    call export(igpnt,x=x,rsdv=rsdv)

    print*,x
    print*,rsdv









    end program testdatahiding
