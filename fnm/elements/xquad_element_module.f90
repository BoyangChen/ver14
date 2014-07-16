module xquad_element_module
    use parameter_module
    use sub2d_element_module
  
  
    implicit none
    private

    integer,parameter :: ndim=2, nndrl=4, nedge=4, nndfl=2*nedge, nnode=nndrl+nndfl, ndof=ndim*nnode
    ! Topology: nodes on each edge; 4 nodes per edge, 1-2 are end nodes, 3-4 are fl. nodes; assigned in lcl node numbers
    integer,parameter :: nodedge(4,nedge)=reshape([1,2,5,6,2,3,7,8,3,4,9,10,4,1,11,12],[4,nedge]) 

    type, public :: xquad_element             ! breakable quadrilateral
        private
        
        integer :: curr_status=0        ! 0 means intact
        integer :: key=0 
        integer :: matkey=0
        real(dp):: theta=zero           ! fibre orientation for lamina
        
        integer :: nodecnc(nnode)=0     ! cnc to glb node arrays for accessing nodal variables (x, u, du, v, dof ...)
        integer :: edgecnc(nedge)=0     ! cnc to glb edge arrays for accessing edge variables (failure status)
        
        type(sub2d_element), allocatable :: subelem(:)
        integer,             allocatable :: subcnc(:,:)      ! sub_elem connec to parent elem nodes
        
    end type xquad_element
  
    interface empty
        module procedure empty_xquad_element
    end interface
  
    interface prepare
        module procedure prepare_xquad_element
    end interface
    
    !~interface precrack
    !~    module procedure precrack_xquad_element
    !~end interface
    
    interface integrate
        module procedure integrate_xquad_element
    end interface
    
    interface extract
        module procedure extract_xquad_element
    end interface




    public :: empty,prepare,integrate,extract



    contains




    ! empty a breakable quadrilateral
    subroutine empty_xquad_element(elem)
  
        type(xquad_element),intent(out) :: elem
        
        elem%curr_status=0
        elem%key=0 
        elem%matkey=0
        elem%theta=zero
        
        elem%nodecnc(nnode)=0
        elem%edgecnc(nedge)=0
        
        if(allocated(subelem)) deallocate(subelem)

    end subroutine empty_xquad_element
  
  
  
    ! this subroutine is used to prepare the connectivity and material lib index of the element
    ! it is used in the initialize_lib_elem procedure in the lib_elem module
    subroutine prepare_xquad_element(elem,key,matkey,theta,nodecnc,edgecnc)
    
        type(xquad_element),    intent(inout)   :: elem
        integer,                intent(in)      :: key
        integer,                intent(in)      :: matkey
        real(dp),               intent(in)      :: theta
        integer,                intent(in)      :: nodecnc(nnode)
        integer,                intent(in)      :: edgecnc(nedge)

        elem%key=key 
        elem%matkey=matkey
        elem%theta=theta
        elem%nodecnc=nodecnc
        elem%edgecnc=edgecnc
    
    end subroutine prepare_xquad_element
    
    
    subroutine extract_xquad_element(elem,curr_status,key,matkey,theta,nodecnc,edgecnc,subelem)
    
        type(xquad_element),                      intent(in)  :: elem
        integer,                        optional, intent(out) :: curr_status
        integer,                        optional, intent(out) :: key
        integer,                        optional, intent(out) :: matkey
        real(dp),                       optional, intent(out) :: theta
        integer,            allocatable,optional, intent(out) :: nodecnc(:)
        integer,            allocatable,optional, intent(out) :: edgecnc(:)
        type(sub2d_element),allocatable,optional, intent(out) :: subelem(:)

        if(present(curr_status)) curr_status=elem%curr_status
        if(present(key)) key=elem%key 
        if(present(matkey)) matkey=elem%matkey
        if(present(theta)) theta=elem%theta
        
        if(present(nodecnc)) then 
            allocate(nodecnc(nnode))
            nodecnc=elem%nodecnc
        end if
        
        if(present(edgecnc)) then 
            allocate(edgecnc(nedge))
            edgecnc=elem%edgecnc
        end if
        
        if(present(subelem)) then
            if(allocated(elem%subelem)) then
                allocate(subelem(size(elem%subelem)))
                subelem=elem%subelem
            end if
        end if
    
    end subroutine extract_xquad_element










!~    subroutine precrack_xquad_element(elem)
!~    use toolkit_module                  ! global tools for element integration
!~    use lib_mat_module                  ! global material library
!~    use lib_node_module                 ! global node library
!~    use lib_edge_module                 ! global edge library
!~    use lib_precrack_module             ! global precrack library
!~    
!~        type(xquad_element),intent(inout)       :: elem
!~        !~integer :: curr_status=0        ! 0 means intact
!~        !~integer :: key=0 
!~        !~integer :: matkey=0
!~        !~real(dp):: theta=zero           ! fibre orientation for lamina
!~        !~
!~        !~integer :: nodecnc(nnode)=0     ! cnc to glb node arrays for accessing nodal variables (x, u, du, v, dof ...)
!~        !~integer :: edgecnc(nedge)=0     ! cnc to glb edge arrays for accessing edge variables (failure status)
!~        !~
!~        !~type(sub2d_element), allocatable :: subelem(:)
!~
!~!==========================================================================
!~!----- check for precrack in this element during 1st increment ------------
!~!==========================================================================
!~        !~if(nprc.gt.0) then
!~        !~  call kcheckprecrack(fstat,pstat,jelem,parent,mcrd,coordsf, &
!~        !~    & nnode,cncsub,mxnd)
!~        !~end if
!~        
!~        
!~    end subroutine precrack_xquad_element










    subroutine integrate_xquad_element(elem, K_matrix, F_vector)
    use toolkit_module                  ! global tools for element integration
    use lib_mat_module                  ! global material library
    use lib_node_module                 ! global node library
    use lib_edge_module                 ! global edge library
    
        type(xquad_element),intent(inout)       :: elem 
        real(kind=dp),allocatable,intent(out)   :: K_matrix(:,:), F_vector(:)
    
    
        ! local variables
        
        real(kind=dp),allocatable       :: Ki(:,:), Fi(:)   ! sub_elem K matrix and F vector
        
        integer :: fstat, fstat0
    
    
        ! initialize K & F
        allocate(K_matrix(ndof,ndof),F_vector(ndof))
        K_matrix=zero; F_vector=zero
        
        fstat=0; fstat0=0
        
        
        fstat=elem%curr_status
        

        !---------------------------------------------------------------------!
        !               update sub element definitions
        !---------------------------------------------------------------------!
     
        if(fstat==5) then 
            ! go straight to sub-element calculations
            continue
        else  
            ! check elem edge status variables and update elem status and sub elem cnc
            call edge_status_partition(elem%edgecnc,nodedge,elem%curr_status,elem%subcnc)         
        end if 

        !---------------------------------------------------------------------!
        !       integrate and assemble sub element system arrays
        !---------------------------------------------------------------------!
        
        if(allocated(elem%subelem)) then
            do i=1, size(elem%subelem)
                ! integrate sub elements
                call integrate(elem%subelem(i),Ki,Fi)               
                ! assemble into global matrix
                call assemble(K_matrix,F_vector,Ki,Fi,elem%subcnc(:,i))
            end do
        else
            write(msg_file,*) 'subelem not allocated in element',elem%key
            call exit_function
        end if


        !---------------------------------------------------------------------!
        !               deallocate local arrays 
        !---------------------------------------------------------------------!
    
 
    end subroutine integrate_xquad_element
  
  
end module xquad_element_module