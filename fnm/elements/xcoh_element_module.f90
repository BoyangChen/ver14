module xcoh_element_module
    use parameter_module
    use glb_clock_module
    use toolkit_module                  ! global tools for element integration
    use lib_edge_module                 ! global edge library
    use lib_node_module                 ! global node library
    use lib_mat_module                  ! global material library
    use coh3d8_element_module
    use subxcoh_element_module
  
  
    implicit none
    private

    integer,parameter :: ndim=3, nndrl=8, nedge=8, nndfl=2*nedge, nnode=nndrl+nndfl, ndof=ndim*nnode
    ! Topology: nodes on each edge; 4 nodes per edge, 1-2 are end nodes, 3-4 are fl. nodes; assigned in lcl node numbers
    integer,parameter :: topo(4,nedge)=reshape([1,2,9,10,2,3,11,12,3,4,13,14,4,1,15,16, &
                                              & 5,6,17,18,6,7,19,20,7,8,21,22,8,5,23,24],[4,nedge])
                                              
    ! element status variable values
    ! in order: transition, refinement, tip, wake, delam-before-edge-break, broken-edges-before-delam
    integer, parameter :: eltrans=1, elref=2, eltip=3, elwake=4, elfail1=5, elfail2=6, elfail3=7
    
    ! edge status variable values
    integer, parameter :: egtrans=1, egref=2, egtip=3, wkcrack=3, cohcrack=4, strgcrack=5
    

    type, public :: xcoh_element             ! breakable brick
        private
        
        integer :: curr_status=0        ! 0 means intact
        integer :: key=0 
        integer :: matkey=0
        
        integer :: nodecnc(nnode)=0     ! cnc to glb node arrays for accessing nodal variables (x, u, du, v, dof ...)
        integer :: edgecnc(nedge)=0     ! cnc to glb edge arrays for accessing edge variables (failure status)
        integer :: ifailedge(nedge)=0   ! indices of failed edges
        
        type(coh3d8_element),  allocatable :: mainelem(:)
        type(subxcoh_element), allocatable :: subelem(:)
        type(int_alloc_array), allocatable :: maincnc(:),subcnc(:)      ! sub_elem connec to parent elem nodes
        
        type(sdv_array), allocatable :: sdv(:)
        
    end type xcoh_element
  
    interface empty
        module procedure empty_xcoh_element
    end interface
  
    interface prepare
        module procedure prepare_xcoh_element
    end interface
    
    interface update
        module procedure update_xcoh_element
    end interface
    
    !~interface precrack
    !~    module procedure precrack_xcoh_element
    !~end interface
    
    interface integrate
        module procedure integrate_xcoh_element
    end interface
    
    interface extract
        module procedure extract_xcoh_element
    end interface




    public :: empty,prepare,integrate,extract



    contains




    ! empty a breakable quadrilateral
    subroutine empty_xcoh_element(elem)
  
        type(xcoh_element),intent(out) :: elem
        
        elem%curr_status=0
        elem%key=0 
        elem%matkey=0
        
        elem%nodecnc=0
        elem%edgecnc=0
        elem%ifailedge=0
        
        
        if(allocated(elem%mainelem)) deallocate(elem%mainelem)
        if(allocated(elem%subelem)) deallocate(elem%subelem)
        if(allocated(elem%maincnc))  deallocate(elem%maincnc)
        if(allocated(elem%subcnc))  deallocate(elem%subcnc)
        if(allocated(elem%sdv)) deallocate(elem%sdv)

    end subroutine empty_xcoh_element
  
  
  
    ! this subroutine is used to prepare the connectivity and material lib index of the element
    ! it is used in the initialize_lib_elem procedure in the lib_elem module
    subroutine prepare_xcoh_element(elem,key,matkey,nodecnc,edgecnc)
    
        type(xcoh_element),    intent(inout)    :: elem
        integer,                intent(in)      :: key
        integer,                intent(in)      :: matkey
        integer,                intent(in)      :: nodecnc(nnode)
        integer,                intent(in)      :: edgecnc(nedge)

        elem%key=key 
        elem%matkey=matkey
        elem%nodecnc=nodecnc
        elem%edgecnc=edgecnc
    
    end subroutine prepare_xcoh_element
    
    
    
    
    ! this subroutine is used to update the ifailedge array of the element
    subroutine update_xcoh_element(elem,ifailedge)
    
        type(xcoh_element),    intent(inout)    :: elem
        integer,                  intent(in)    :: ifailedge(nedge)

        elem%ifailedge=ifailedge
    
    end subroutine update_xcoh_element
    
    
    
    
    subroutine extract_xcoh_element(elem,curr_status,key,matkey,nodecnc,edgecnc, &
    & ifailedge,mainelem,subelem,maincnc,subcnc,sdv)
    
        type(xcoh_element),                      intent(in)  :: elem
        integer,                        optional, intent(out) :: curr_status
        integer,                        optional, intent(out) :: key
        integer,                        optional, intent(out) :: matkey
        integer,            allocatable,optional, intent(out) :: nodecnc(:)
        integer,            allocatable,optional, intent(out) :: edgecnc(:)
        integer,            allocatable,optional, intent(out) :: ifailedge(:)
        type(coh3d8_element),allocatable,optional,intent(out) :: mainelem(:)
        type(subxcoh_element),allocatable,optional, intent(out) :: subelem(:)
        type(int_alloc_array),allocatable,optional,intent(out):: maincnc(:),subcnc(:)
        type(sdv_array),    allocatable,optional, intent(out) :: sdv(:)

        if(present(curr_status)) curr_status=elem%curr_status
        if(present(key)) key=elem%key 
        if(present(matkey)) matkey=elem%matkey
        
        if(present(nodecnc)) then 
            allocate(nodecnc(nnode))
            nodecnc=elem%nodecnc
        end if
        
        if(present(edgecnc)) then 
            allocate(edgecnc(nedge))
            edgecnc=elem%edgecnc
        end if

        if(present(ifailedge)) then 
            allocate(ifailedge(nedge))
            ifailedge=elem%ifailedge
        end if
        
        if(present(mainelem)) then
            if(allocated(elem%mainelem)) then
                allocate(mainelem(size(elem%mainelem)))
                mainelem=elem%mainelem
            end if
        end if
        
        if(present(subelem)) then
            if(allocated(elem%subelem)) then
                allocate(subelem(size(elem%subelem)))
                subelem=elem%subelem
            end if
        end if
        
        
        if(present(maincnc)) then
            if(allocated(elem%maincnc)) then
                allocate(maincnc(size(elem%maincnc)))
                maincnc=elem%maincnc
            end if
        end if
        
        
        if(present(subcnc)) then
            if(allocated(elem%subcnc)) then
                allocate(subcnc(size(elem%subcnc)))
                subcnc=elem%subcnc
            end if
        end if
        
        if(present(sdv)) then        
            if(allocated(elem%sdv)) then
                allocate(sdv(size(elem%sdv)))
                sdv=elem%sdv
            end if
        end if
    
    end subroutine extract_xcoh_element






    subroutine integrate_xcoh_element(elem, K_matrix, F_vector)
    
        type(xcoh_element),intent(inout)       :: elem 
        real(kind=dp),allocatable,intent(out)   :: K_matrix(:,:), F_vector(:)
    
    
        ! local variables
        type(int_alloc_array), allocatable  :: mainglbcnc(:),subglbcnc(:),subedgecnc(:)
        real(kind=dp),allocatable           :: Ki(:,:), Fi(:)   ! sub_elem K matrix and F vector
        
        integer :: i,j,l, elstat, mainelstat, subelstat1, subelstat2, nfe1, nfe2
        integer, allocatable :: dofcnc(:), ifailedge1(:), ifailedge2(:)
        
        
          
    
        ! initialize K & F
        allocate(K_matrix(ndof,ndof),F_vector(ndof))
        K_matrix=zero; F_vector=zero
        
        ! initialize local variables
        i=0; j=0; l=0
        elstat=0; mainelstat=0; subelstat1=0; subelstat2=0

        
        

        ! extract current status value
        elstat=elem%curr_status  

        ! if elem is intact
        if(elstat==intact) then
        
            ! assign 1 coh3d8 elem as the main elem before failure, if not yet done
            if(.not.allocated(elem%mainelem)) then 
                allocate(elem%mainelem(1))
                allocate(elem%maincnc(1))
                allocate(elem%maincnc(1)%array(nndrl))   ! coh3d8 elem
                allocate(mainglbcnc(1))
                allocate(mainglbcnc(1)%array(nndrl))
                ! main elm 1 connec
                elem%maincnc(1)%array=[(i, i=1,nndrl)]
                mainglbcnc(1)%array(:)=elem%nodecnc(elem%maincnc(1)%array(:))
                ! create sub elements
                call prepare(elem%mainelem(1),key=0,connec=mainglbcnc(1)%array,matkey=elem%matkey)
                
            end if
            
            ! check if elem has started to fail; if so, no more edge status partitioning later
            call extract(elem%mainelem(1),curr_status=mainelstat)
            
            if(mainelstat>intact) then
            ! if elem has reached failure onset, then update curr status; no edge status partition
                elstat=elfail1
                elem%curr_status=elstat
                ! reaching here, elem curr status is elfail1
            else
            ! if elem has not reached failure initiation, then
            ! check edge status, and partition into 2 subxcoh elems if any edge status is not intact
                if(maxval(elem%ifailedge)>0) then
                    elstat=elfail2
                    elem%curr_status=elstat
                    
                    ! deallocate original elem
                    deallocate(elem%mainelem)
                    deallocate(elem%maincnc)
                    
                    ! allocate two subxcoh elems
                    allocate(elem%subelem(2))
                    allocate(elem%subcnc(2))
                    allocate(subglbcnc(2))
                    allocate(subedgecnc(2))
                    do i=1, 2
                        allocate(elem%subcnc(i)%array(16))
                        allocate(subglbcnc(i)%array(16))
                        allocate(subedgecnc(i)%array(4))
                        elem%subcnc(i)%array=0
                        subglbcnc(i)%array=0
                        subedgecnc(i)%array=0
                    end do
                    
                    ! local connec of two subxcoh elems
                    elem%subcnc(1)%array=[1,2,3,4,5,6,7,8,17,18,19,20,21,22,23,24]
                    elem%subcnc(2)%array=[6,5,8,7,2,1,4,3,10,9,16,15,14,13,12,11]
                    
                    ! glb connec of two subxcoh elems
                    subglbcnc(1)%array(:)=elem%nodecnc(elem%subcnc(1)%array(:))
                    subglbcnc(2)%array(:)=elem%nodecnc(elem%subcnc(2)%array(:))
                    
                    ! glb edge cnc of two subxcoh elems
                    subedgecnc(1)%array=elem%edgecnc([5,6,7,8])
                    subedgecnc(2)%array=elem%edgecnc([1,4,3,2])
                    
                    ! prepare two subxcoh elems
                    call prepare(elem%subelem(1),key=0,matkey=elem%matkey,&
                    & nodecnc=subglbcnc(1)%array,edgecnc=subedgecnc(1)%array)
                    call prepare(elem%subelem(2),key=0,matkey=elem%matkey,&
                    & nodecnc=subglbcnc(2)%array,edgecnc=subedgecnc(2)%array)
                    
                end if
                ! reaching here, elem curr status is either intact or elfail2
            end if  
         
        end if
        ! reaching here, elem curr status is either intact, elfail1 or efail2       
            

        ! if elem has already been partitioned into 2 subxcoh elems,
        ! update their ifailedge arrays and elem curr status
        if(elstat==elfail2) then 
        
            call extract(elem%subelem(1),curr_status=subelstat1)
            call extract(elem%subelem(2),curr_status=subelstat2)
            
            ! if both subxcoh elems have reached final partition state, 
            ! then no update is needed; break out of the control statement
            if(subelstat1>=elfail1 .and. subelstat2>=elfail1) then
                elstat=elfail3
                elem%curr_status=elstat
                exit
            end if
        
            ! update ifailedge arrays of two subxcoh elems
            allocate(ifailedge1(4)); ifailedge1=0
            allocate(ifailedge2(4)); ifailedge2=0
            
            nfe1=0
            nfe2=0
            do i=1, nedge
                select case (elem%ifailedge(i))
                    case(1) ! edge 1 here is edge 1 of subxcoh2
                        nfe2=nfe2+1
                        ifailedge2(nfe2)=1
                    case(2) ! edge 2 here is edge 4 of subxcoh2
                        nfe2=nfe2+1
                        ifailedge2(nfe2)=4
                    case(3) ! edge 3 here is edge 3 of subxcoh2
                        nfe2=nfe2+1
                        ifailedge2(nfe2)=3
                    case(4) ! edge 4 here is edge 2 of subxcoh2
                        nfe2=nfe2+1
                        ifailedge2(nfe2)=2
                    case(5:8) ! top 4 edges are the 4 edges of subxcoh1
                        nfe1=nfe1+1
                        ifailedge1(nfe1)=elem%ifailedge(i)-4
                    case(0)
                    ! do nothing
                        continue
                    case default
                        write(msg_file,*)'sth wrong in xcoh integration subxcoh ifailedge update!'
                        call exit_function            
                end select   
            end do
            
            if(subelstat1<elfail1) call update(elem%subelem(1),ifailedge=ifailedge1)
            if(subelstat2<elfail1) call update(elem%subelem(2),ifailedge=ifailedge2)   
        end if


        !---------------------------------------------------------------------!
        !       integrate and assemble sub element system arrays
        !---------------------------------------------------------------------!        

        
        ! empty K and F for reuse
        K_matrix=zero; F_vector=zero  
     
        ! integrate sub elements and assemble into global matrix
        ! if elem partition just changed in this iteration, no failure modelling at this iteration
        if(allocated(elem%mainelem)) then
        
            call integrate(elem%mainelem(1),Ki,Fi)
            if(allocated(dofcnc)) deallocate(dofcnc)
            allocate(dofcnc(size(Fi))); dofcnc=0
            do j=1, size(elem%maincnc(1)%array) ! no. of nodes in sub elem i
                do l=1, ndim
                    ! dof indices of the jth node of sub elem i 
                    dofcnc((j-1)*ndim+l)=(elem%maincnc(1)%array(j)-1)*ndim+l
                end do
            end do
            call assembleKF(K_matrix,F_vector,Ki,Fi,dofcnc)
            deallocate(Ki)
            deallocate(Fi)
            deallocate(dofcnc)
        
        else if(allocated(elem%subelem)) then
            
            do i=1, size(elem%subelem)
                call integrate(elem%subelem(i),Ki,Fi)
                if(allocated(dofcnc)) deallocate(dofcnc)
                allocate(dofcnc(size(Fi))); dofcnc=0
                do j=1, size(elem%subcnc(i)%array) ! no. of nodes in sub elem i
                    do l=1, ndim
                        ! dof indices of the jth node of sub elem i 
                        dofcnc((j-1)*ndim+l)=(elem%subcnc(i)%array(j)-1)*ndim+l
                    end do
                end do
                call assembleKF(K_matrix,F_vector,Ki,Fi,dofcnc)
                deallocate(Ki)
                deallocate(Fi)
                deallocate(dofcnc)
            end do

        else
            write(msg_file,*)'elem not allocated in xcoh element module!'
            call exit_function
        end if
                
        
        !---------------------------------------------------------------------!
        !               deallocate local arrays 
        !---------------------------------------------------------------------!
        if(allocated(Ki)) deallocate(Ki)
        if(allocated(Fi)) deallocate(Fi)
        if(allocated(mainglbcnc)) deallocate(mainglbcnc)
        if(allocated(subglbcnc)) deallocate(subglbcnc)
        if(allocated(subedgecnc)) deallocate(subedgecnc)
        if(allocated(dofcnc)) deallocate(dofcnc)
        if(allocated(ifailedge1)) deallocate(ifailedge1)
        if(allocated(ifailedge2)) deallocate(ifailedge2)
    
 
    end subroutine integrate_xcoh_element
    
     
  
end module xcoh_element_module
