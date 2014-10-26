module xlam_element_module
    use parameter_module
    use toolkit_module                  ! global tools for element integration
    use lib_edge_module                 ! global edge library
    use lib_node_module                 ! global node library
    use lib_mat_module                  ! global material library
    use xbrick_element_module
    use coh3d8_element_module

    implicit none
    private
    
    ! parameters for no. nodes of ply-block element (xbrick) and interface element (coh3d8)
    integer, parameter :: ndim=3, nndplyblk=24, nedgplyblk=8, nndinterf=8
    

    type, public :: xlam_element
        private
        
        integer :: curr_status=0        ! 0 means intact
        integer :: key=0 
        integer :: bulkmat=0
        integer :: cohmat=0
        integer :: interfmat=0
        
        integer,allocatable :: nodecnc(:)    ! cnc to glb node arrays for accessing nodal variables (x, u, du, v, dof ...)
        integer,allocatable :: edgecnc(:)
        real(dp),allocatable:: layup(:,:)   ! angle and relative thickness of all plies
    
        type(xbrick_element),allocatable :: plyblk(:)
        type(coh3d8_element),allocatable :: interf(:)
        
        type(int_alloc_array), allocatable :: plyblknodecnc(:), plyblkedgecnc(:)      ! plyblk_elem connec to parent elem nodes
        type(int_alloc_array), allocatable :: interfcnc(:)      ! plyblk_elem connec to parent elem nodes
    
    end type xlam_element
    
    interface empty
        module procedure empty_xlam_element
    end interface
  
    interface prepare
        module procedure prepare_xlam_element
    end interface
    
    interface integrate
        module procedure integrate_xlam_element
    end interface
    
    interface extract
        module procedure extract_xlam_element
    end interface




    public :: empty,prepare,integrate,extract
    
    
    
    
    contains
    
    
    
    
    
    subroutine empty_xlam_element(elem)
  
        type(xlam_element),intent(out) :: elem
        
        elem%curr_status=0
        elem%key=0 
        elem%bulkmat=0
        elem%cohmat=0
        elem%interfmat=0
        
        if(allocated(elem%nodecnc)) deallocate(elem%nodecnc)
        if(allocated(elem%edgecnc)) deallocate(elem%edgecnc)
        if(allocated(elem%layup)) deallocate(elem%layup)
        if(allocated(elem%plyblk)) deallocate(elem%plyblk)
        if(allocated(elem%interf)) deallocate(elem%interf)
        if(allocated(elem%plyblknodecnc)) deallocate(elem%plyblknodecnc)
        if(allocated(elem%plyblkedgecnc)) deallocate(elem%plyblkedgecnc)
        if(allocated(elem%interfcnc)) deallocate(elem%interfcnc)

    end subroutine empty_xlam_element
  
    
    
    
    
    
    subroutine extract_xlam_element(elem,curr_status,key,bulkmat,cohmat,interfmat,nodecnc,edgecnc,layup &
    & ,plyblk,interf,plyblknodecnc,plyblkedgecnc,interfcnc)
    
        type(xlam_element),intent(in)  :: elem
        
        integer, optional, intent(out) :: curr_status
        integer, optional, intent(out) :: key 
        integer, optional, intent(out) :: bulkmat
        integer, optional, intent(out) :: cohmat
        integer, optional, intent(out) :: interfmat
        
        integer,allocatable, optional, intent(out) :: nodecnc(:) 
        integer,allocatable, optional, intent(out) :: edgecnc(:)
        real(dp),allocatable, optional, intent(out):: layup(:,:)
    
        type(xbrick_element),allocatable, optional, intent(out) :: plyblk(:)
        type(coh3d8_element),allocatable, optional, intent(out) :: interf(:)
        
        type(int_alloc_array), allocatable, optional, intent(out) :: plyblknodecnc(:), plyblkedgecnc(:)
        type(int_alloc_array), allocatable, optional, intent(out) :: interfcnc(:)
        
        
        
        if(present(curr_status)) curr_status=elem%curr_status
        if(present(key)) key=elem%key 
        if(present(bulkmat)) bulkmat=elem%bulkmat
        if(present(cohmat)) cohmat=elem%cohmat
        if(present(interfmat)) interfmat=elem%interfmat
        
        if(present(nodecnc)) then 
            if(allocated(elem%nodecnc)) then
                allocate(nodecnc(size(elem%nodecnc)))
                nodecnc=elem%nodecnc
            end if
        end if
        
        if(present(edgecnc)) then 
            if(allocated(elem%edgecnc)) then
                allocate(edgecnc(size(elem%edgecnc)))
                edgecnc=elem%edgecnc
            end if
        end if
        
        if(present(layup)) then
            if(allocated(elem%layup)) then
                allocate(layup(size(elem%layup(:,1)),size(elem%layup(1,:))))
                layup=elem%layup
            end if
        end if
        
        if(present(plyblk)) then
            if(allocated(elem%plyblk)) then
                allocate(plyblk(size(elem%plyblk)))
                plyblk=elem%plyblk
            end if
        end if
        
        if(present(interf)) then
            if(allocated(elem%interf)) then
                allocate(interf(size(elem%interf)))
                interf=elem%interf
            end if
        end if
        
        if(present(plyblknodecnc)) then
            if(allocated(elem%plyblknodecnc)) then
                allocate( plyblknodecnc(size(elem%plyblknodecnc)))
                plyblknodecnc=elem%plyblknodecnc
            end if
        end if
        
        if(present(plyblkedgecnc)) then
            if(allocated(elem%plyblkedgecnc)) then
                allocate( plyblkedgecnc(size(elem%plyblkedgecnc)))
                plyblkedgecnc=elem%plyblkedgecnc
            end if
        end if
        
        if(present(interfcnc)) then
            if(allocated(elem%interfcnc)) then
                allocate( interfcnc(size(elem%interfcnc)))
                interfcnc=elem%interfcnc
            end if
        end if
    
    end subroutine extract_xlam_element
    
    
    
    
    
    subroutine prepare_xlam_element(elem,key,bulkmat,cohmat,interfmat,nodecnc,edgecnc,layup)
    
        type(xlam_element),    intent(inout)    :: elem
        integer,                intent(in)      :: key
        integer,                intent(in)      :: bulkmat, cohmat, interfmat
        integer,                intent(in)      :: nodecnc(:)
        integer,                intent(in)      :: edgecnc(:)
        real(dp),               intent(in)      :: layup(:,:)

        elem%key=key 
        elem%bulkmat=bulkmat
        elem%cohmat=cohmat
        elem%interfmat=interfmat
        
        if (size(nodecnc)/=nndplyblk*size(layup(1,:))) then
            write(msg_file,*)'layup and no. nodes do not match in prepare_xlam!'
            call exit_function
        end if
        
        if (size(edgcnc)/=nedgplyblk*size(layup(1,:))) then
            write(msg_file,*)'layup and no. edges do not match in prepare_xlam!'
            call exit_function
        end if
        
        allocate(elem%nodecnc(size(nodecnc)))
        allocate(elem%edgecnc(size(edgecnc)))
        allocate(elem%layup(size(layup(:,1)),size(layup(1,:))))
        elem%nodecnc=nodecnc
        elem%edgecnc=edgecnc
        elem%layup=layup
    
    end subroutine prepare_xlam_element
    
    
    


    subroutine integrate_xlam_element(elem, K_matrix, F_vector)
    
        type(xlam_element),intent(inout)       :: elem 
        real(kind=dp),allocatable,intent(out)   :: K_matrix(:,:), F_vector(:)
    
    
        ! local variables
        
        ! sub_elem K matrix and F vector
        real(kind=dp),allocatable           :: Ki(:,:), Fi(:)
        
        ! loop counters
        integer :: i,j,l  

        ! no. dof, no. plyblocks and no. interfaces in this elem
        integer :: ndof, nplyblk, ninterf
        
        ! lcl arrays to store temporarily the glb cnc of plyblock nodes, edges, and interface nodes
        integer :: plyblknode(nndplyblk), plyblkedge(nedgplyblk), interfnode(nndinterf) 
        
        ! lcl arrays to store temporarily the cnc of dofs of each sub elem to parent elem dofs
        integer, allocatable :: dofcnc(:)
       
        
        ! initialize local variables
        i=0; j=0; l=0
        ndof=0; nplyblk=0; ninterf=0
        plyblknode=0; plyblkedge=0; interfnode=0
        
        
        ! check if the elem has been prepared (by checking layup; note that in prepare subroutine all attributes 
        ! must be allocated together, so checking one of them (here layup) would suffice)
        if(.not.allocated(elem%layup)) then
            write(msg_file,*)'xlam element must be prepared before integration'
            call exit_function
        end if

        ! extract no. plyblock and no. interfaces from layup, and calculate ndof
        nplyblk=size(elem%layup(1,:))
        ninterf=nplyblk-1
        ndof=ndim*size(elem%nodecnc)
        

        !---------------------------------------------------------------------!
        !       prepare sub elements
        !---------------------------------------------------------------------! 
        if (.not.allocated(elem%plyblk)) then
        ! if plyblock elems (& consequently interface elems) not yet allocated (beginning of analysis), 
        ! allocate the plyblock and interface elems first
            
            ! allocate plyblk elems and interface elems
            allocate(elem%plyblk(nplyblk))
            allocate(elem%interf(ninterf))
            
            ! allocate plyblk node and edge cnc arrays...
            allocate(elem%plyblknodecnc(nplyblk))
            allocate(elem%plyblkedgecnc(nplyblk))
            
            do i=1, nplyblk
                allocate(elem%plyblknodecnc(i)%array(nndplyblk))
                allocate(elem%plyblkedgecnc(i)%array(nedgplyblk))
                elem%plyblknodecnc(i)%array=[(i-1)*nndplyblk+1 : i*nndplyblk]
                elem%plyblkedgecnc(i)%array=[(i-1)*nedgplyblk+1 : i*nedgplyblk]
            end do
            
            ! ...and interface node cnc arrays
            allocate(elem%interfcnc(ninterf))
            
            do i=1, ninterf
                allocate(elem%interfcnc(i)%array(nndinterf))               
                ! 1st half of interface nodes comes from bottom plyblk elem top surface
                elem%interfcnc(i)%array(1 : nndinterf/2)=&
                & [(i-1)*nndplyblk+nndplyblk/2+1 : (i-1)*nndplyblk+nndplyblk/2+nndinterf/2]
                ! 2nd half of interface nodes comes from top plyblk elem bottom surface
                elem%interfcnc(i)%array(nndinterf/2+1 : nndinterf)=&
                & [i*nndplyblk+1 : i*nndplyblk+nndinterf/2]           
            end do
            
            
            
            ! prepare plyblk elems
            do i=1, nplyblk
                ! extract the glb node and edge cnc of this plyblock element from elem glb cnc and plyblk i's local cnc
                plyblknode(:)=elem%nodecnc(elem%plyblknodecnc(i)%array(:))
                plyblkedge(:)=elem%edgecnc(elem%plyblkedgecnc(i)%array(:))
                
                ! prepare each plyblk elem (here xbrick elem type)
                call prepare(elem%plyblk(i),key=0,bulkmat=elem%bulkmat,cohmat=elem%cohmat, &
                & nodecnc=plyblknode,edgecnc=plyblkedge)
            end do
            
            ! prepare interf elems
            do i=1, ninterf
                ! extract the glb node cnc of this interface element from elem glb cnc and interface i's local cnc
                interfnode(:)=elem%nodecnc(elem%interfcnc(i)%array(:))
                
                ! prepare each interface elem (here coh3d8 elem type)
                call prepare(elem%interf(i),key=0,connec=interfnode,matkey=elem%interfmat)
            end do
        
        end if
        

        !---------------------------------------------------------------------!
        !       integrate and assemble sub element system arrays
        !---------------------------------------------------------------------!

        ! initialize K & F
        allocate(K_matrix(ndof,ndof),F_vector(ndof))
        K_matrix=zero; F_vector=zero 
     
        ! integrate plyblock elements and assemble into global matrix

        do i=1, nplyblk
            call integrate(elem%plyblk(i),Ki,Fi)
            if(allocated(dofcnc)) deallocate(dofcnc)
            allocate(dofcnc(size(Fi))); dofcnc=0
            do j=1, nndplyblk ! no. of nodes in sub elem i
                do l=1, ndim
                    ! dof indices of the jth node of sub elem i 
                    dofcnc((j-1)*ndim+l)=(elem%plyblknodecnc(i)%array(j)-1)*ndim+l
                end do
            end do
            call assembleKF(K_matrix,F_vector,Ki,Fi,dofcnc)
            deallocate(Ki)
            deallocate(Fi)
            deallocate(dofcnc)
        end do

        ! integrate cohesive elements and assemble into global matrix

        do i=1, ninterf
            call integrate(elem%interf(i),Ki,Fi)
            if(allocated(dofcnc)) deallocate(dofcnc)
            allocate(dofcnc(size(Fi))); dofcnc=0
            do j=1, nndinterf ! no. of nodes in sub elem i
                do l=1, ndim
                    ! dof indices of the jth node of sub elem i 
                    dofcnc((j-1)*ndim+l)=(elem%interfcnc(i)%array(j)-1)*ndim+l
                end do
            end do
            call assembleKF(K_matrix,F_vector,Ki,Fi,dofcnc)
            deallocate(Ki)
            deallocate(Fi)
            deallocate(dofcnc)
        end do
        
        
        
        !---------------------------------------------------------------------!
        !               deallocate local arrays 
        !---------------------------------------------------------------------!
        if(allocated(Ki)) deallocate(Ki)
        if(allocated(Fi)) deallocate(Fi)
        if(allocated(dofcnc)) deallocate(dofcnc)
    
 
    end subroutine integrate_xlam_element
    
    
    

end module xlam_element_module