module xquad_element_module
    use parameter_module
    use sub2d_element_module
  
  
    implicit none
    private

    integer,parameter :: ndim=2, nndrl=4, nedge=4, nndfl=2*nedge, nnode=nndrl+nndfl, ndof=ndim*nnode
    ! Topology: nodes on each edge; 4 nodes per edge, 1-2 are end nodes, 3-4 are fl. nodes; assigned in lcl node numbers
    integer,parameter :: edgenode(4,nedge)=reshape([1,2,5,6,2,3,7,8,3,4,9,10,4,1,11,12],[4,nedge]) 

    type, public :: xquad_element             ! breakable quadrilateral
        private
        
        integer :: curr_status=0        ! 0 means intact
        integer :: key=0 
        integer :: matkey=0
        real(dp):: theta=zero           ! fibre orientation for lamina
        
        integer :: nodecnc(nnode)=0     ! cnc to glb node arrays for accessing nodal variables (x, u, du, v, dof ...)
        integer :: edgecnc(nedge)=0     ! cnc to glb edge arrays for accessing edge variables (failure status)
        
        type(sub2d_element), allocatable :: subelem(:)
        type(int_array)      allocatable :: subcnc(:)      ! sub_elem connec to parent elem nodes
        
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






    subroutine integrate_xquad_element(elem, K_matrix, F_vector)
    use toolkit_module                  ! global tools for element integration
    !~use lib_mat_module                  ! global material library
    !~use lib_node_module                 ! global node library
    !~use lib_edge_module                 ! global edge library
    
        type(xquad_element),intent(inout)       :: elem 
        real(kind=dp),allocatable,intent(out)   :: K_matrix(:,:), F_vector(:)
    
    
        ! local variables
        
        real(kind=dp),allocatable       :: Ki(:,:), Fi(:)   ! sub_elem K matrix and F vector
        
        integer :: i,j,l
    
    
        ! initialize K & F
        allocate(K_matrix(ndof,ndof),F_vector(ndof))
        K_matrix=zero; F_vector=zero
        
        ! initialize local variables
        i=0; j=0; l=0

        

        !---------------------------------------------------------------------!
        !               update sub element definitions
        !---------------------------------------------------------------------!
     
        if(elem%curr_status==5) then 
            ! xelement already cracked
            ! go straight to sub-element calculations
            continue
        else  
            ! check elem edge status variables and update elem status and sub elem cnc
            call edge_status_partition(elem%edgecnc,edgenode,elem%curr_status,elem%subcnc)         
        end if 

        !---------------------------------------------------------------------!
        !       integrate and assemble sub element system arrays
        !---------------------------------------------------------------------!
        
        if(allocated(elem%subelem)) then
            do i=1, size(elem%subelem)
                ! integrate sub elements
                call integrate(elem%subelem(i),Ki,Fi)               
                ! assemble into global matrix
                call assembleKF(K_matrix,F_vector,Ki,Fi,elem%subcnc(i)%array)
            end do
        else
            write(msg_file,*) 'subelem not allocated in element',elem%key
            call exit_function
        end if


        !---------------------------------------------------------------------!
        !               deallocate local arrays 
        !---------------------------------------------------------------------!
        deallocate(Ki,Fi)
    
 
    end subroutine integrate_xquad_element
    
    
    
    
    
    
    
    subroutine edge_status_partition(elem)
    use lib_edge_module                 ! global edge library
    use lib_node_module                 ! global node library
    
    ! passed-in variables
    type(xquad_element), intent(inout) :: elem

    
    ! local variable
    integer :: edgstat          ! edge status variable
    integer :: fedg(nedge)      ! status variable array of element edges
    integer :: nfedg            ! no. of failed edges in the element
    integer :: ifedg(nedge)     ! index of failed edges in the element
    integer :: fstat2           ! local copy of fstat
    integer :: jbe1,jbe2, jnode ! indices of 2 broken edges, and a variable to hold a glb node index
    integer :: iscross          ! indicator of line intersection; >0 if two lines intersect
    integer :: i, j, l          ! counters
    
    real(dp), allocatable :: coord(:)   ! nodal coord array to store the coords of a node extracted from glb node lib
    real(dp) :: xp1, yp1, xp2, yp2      ! (x,y) of point 1 and point 2 on the crack line
    real(dp) :: x1, y1, x2, y2          ! (x,y) of node 1 and node 2 of an element edge
    real(dp) :: xct, yct                ! (x,y) of a crack tip on an edge, i.e., intersection of crack line & edge
    
    ! --------------------------------------------------------------------!
    !       *** workings of edgstat, fedg, nfedg, ifedg ***
    !
    !       e.g.: element edge 1 and 3 are broken, then:
    !
    !           - nfedg=2
    !           - fedg(1)>0; fedg(2)=0; fedg(3)>0; fedg(4)=0
    !           - ifedg(1)=1; ifedg(2)=3; ifedg(3:)=0
    !
    ! --------------------------------------------------------------------!
    
    
    ! initialize local variables
    
    edgstat=0; fedg=0; nfedg=0; ifedg=0; fstat2=0
    jbe1=0; jbe2=0; jnode=0
    iscross=0
    i=0; j=0; l=0
    
    xp1=zero; yp1=zero
    xp2=zero; yp2=zero

    x1=zero; y1=zero
    x2=zero; y2=zero
    
    xct=zero; yct=zero


    
!       find and store the broken edges' variables

        do i=1,nedge
            ! extract edge status variable from global edge library
            edgstat=lib_edge(elem%edgecnc(i))
            ! check and update element edge variables/arrays
            if(edgstat/=0) then
                fedg(i)=edgstat ! update edge status array
                nfedg=nfedg+1   ! update total no. of damaged edges
                ifedg(nfedg)=i  ! update the indices of damaged edges
            end if
        end do
     



!       calculate fstat value from edge status variables

10      if(nfedg==0) then
            ! no edge failed/damaged, do nothing
            continue
            
        else if(nfedg==1) then 
        ! could be 1st time wake elm, tip elm, ref elm and trans elm
        ! update the edge status, crack tip coords and fstat accordingly
        
            if(fedg(ifedg(1))==1) then
              ! edge marks the refinement end and the trans elem start 
              ! elem is a trans elem, only this edge needs to be partitioned
                fstat2=1
                
            else
              ! another edge must be partitioned to form a ref/tip/wake elem
              ! find the other edge to be partitioned
                
                ! first, find the index of the broken edge
                jbe1=ifedg(1)
                
                ! find the smaller fl. node on this edge
                jnode=min(elem%nodecnc(edgenode(3,jbe1)),elem%nodecnc(edgenode(4,jbe1)))
                
                ! find the coordinates of this fl. node (crack tip coords)
                call extract(lib_node(jnode),x=coord)
                ! store x,y values of this node in xp1, yp1 (legacy format)
                xp1=coord(1)
                yp1=coord(2)

                ! from theta, calculate another point on the crack line (could be any point along the line)
                xp2=xp1+cos(elem%theta/halfcirc*pi)
                yp2=yp1+sin(elem%theta/halfcirc*pi)
                
                ! next, find the other edge crossed by the crack line
                do i=1,nedge
                    if (i==jbe1) cycle ! the already broken edge, go to next edge
                    
                    ! extract end node 1 coords of edge i
                    jnode=elem%nodecnc(edgenode(1,i))
                    call extract(lib_node(jnode),x=coord)
                    x1=coord(1)
                    y1=coord(2)
                    
                    ! extract end node 2 coords of edge i
                    jnode=elem%nodecnc(edgenode(2,i))
                    call extract(lib_node(jnode),x=coord)
                    x2=coord(1)
                    y2=coord(2)

                    ! zero cross status and intersection coords for reuse
                    iscross=0
                    xct=zero
                    yct=zero
                    
                    ! check intersection of the crack line and the edge
                    call klinecross(x1,y1,x2,y2,xp1,yp1,xp2,yp2,iscross,xct,yct)
                    if (iscross>0) exit ! found the edge, no need to proceed
                end do
                
                ! update nfedg & ifedg
                nfedg=2     ! no. of broken edge is now 2
                ifedg(2)=i  ! index of the 2nd broken edge is i
                jbe2=i      ! store it in jbe2 (safer)
                
                ! update the two fl. node coords on this edge
                jnode=elem%nodecnc(edgenode(3,jbe2))
                call update(lib_node(jnode),x=[xct,yct])
                jnode=elem%nodecnc(edgenode(4,jbe2))               
                call update(lib_node(jnode),x=[xct,yct])
                
                ! update fnode array of the fnode on edge i
                if(fedg(jbe1)==2) then ! tip elem end, refinement elem. start (1st time)
                    fstat2=2               
                    ! change 2nd broken edge status to 1 (trans elem start)
                    fedg(jbe2)=1
                    lib_edge(elem%edgecnc(jbe2))=1
             
                else if(fedg(jbe1)==3) then ! wake elem end, tip elem start (1st time)
                    fstat2=3
                    ! change 2nd broken edge status to 2 (refinement start)
                    fedg(jbe2)=2
                    lib_edge(elem%edgecnc(jbe2))=2
               
                else if(fedg(jbe1)>=4) then ! wake elem start (1st time)
                    fstat2=4 !wake elem               
                    ! change 2nd broken edge status to 3 (tip elem start)
                    fedg(jbe2)=3
                    lib_edge(elem%edgecnc(jbe2))=3
                    
                else ! unknown edge status
                    write(msg_file,*)'unknown edge status!'
                    call exit_function
                end if        
            endif
                
        else if(nfedg==2) then 
        ! could be cracked, wake, tip, refinement elem
        ! only update the fstat, not the edge status, nor the crack tip coords
            jbe1=ifedg(1)
            jbe2=ifedg(2)
            if(fedg(jbe1)<=2 .and. fedg(jbe2)<=2) then
            ! refinement elem
                fstat2=2         
            else if((fedg(jbe1)<=3 .and. fedg(jbe2)==3).or.(fedg(jbe2)<=3 .and. fedg(jbe1)==3)) then
            ! tip elem
                fstat2=3
            else if((fedg(jbe1)<4 .and. fedg(jbe2)>=4).or.(fedg(jbe2)<4 .and. fedg(jbe1)>=4)) then
            ! wake elem, cohesive/stress-free crack
                fstat2=4
            else if(fedg(jbe1)>=4 .and. fedg(jbe2)>=4) then
            ! cracked elem, cohesive/stress-free crack
                fstat2=5        
            else ! unknown combination
                write(msg_file,*)'unknown combination of 2 edge status!'
                call exit_function
            end if
          
        else if(nfedg>2) then
        ! not yet supported  
        
            write(msg_file,*)'more than 2 broken edges not yet supported!'
          
            ! partition only according to the first two broken edges
            nfedg=2
            goto 10
          
        else
        
            write(msg_file,*)'unknown no. of nfedg!'
            call exit_function
          
        end if
                



            
!       update element curr_status and sub-element cnc matrices

        if(fstat2>elem%curr_status) then
            elem%curr_status=fstat2                       
            call update_subcnc(elem,fedg,ifedg,nfedg)
        end if


!       deallocate local dynamic arrays

        deallocate(fedg)
        deallocate(ifedg)
    
    
    end subroutine edge_status_partition
    
  


       
    subroutine update_subcnc(elem,fedg,ifedg,nfedg)
    
    ! passed-in variables
    type(xquad_element),    intent(inout)   :: elem
    integer,                intent(in)      :: fedg(:), ifedg(:), nfedg



    ! local variables
    integer:: i,i1,i2,i3, nsub, jnode



!       initialize local variables

        i=0; i1=0; i2=0; i3=0; nsub=0; jnode=0
        


10      select case (nfedg)
            case (0) !- no cracked edge, do nothing
                continue
            
            
            case (1) !- one edge cracked, trans partition
                ! find the index of the broken edge
                ibe=ifedg(1)

                ! verify its status variable value
                if(fedg(ibe)/=1) then
                    write(msg_file,*)'transition partition only accepts fedg=1!'
                    call exit_function
                end if
                
                ! allocate sub element cnc arrays; in this case, 3 tri elements
                nsub=3
                allocate(elem%subcnc(nsub))
                do j=1, nsub
                    allocate(elem%subcnc(j)%array(3))
                end do

                ! find the neighbouring edges of this broken edge, in counter-clockwise direction
                select case(ibe)
                    case (1)
                        i1=2;i2=3;i3=4
                    case (2)
                        i1=3;i2=4;i3=1
                    case (3)
                        i1=4;i2=1;i3=2
                    case (4)
                        i1=1;i2=2;i3=3
                    case default
                        write(msg_file,*)'wrong broken edge in tip partition, subcnc'
                        call exit_function
                end select
                
                ! find the smaller glb node on the broken edge
                if(elem%nodecnc(edgenode(3,ibe))<elem%nodecnc(edgenode(4,ibe))) then
                    jnode=edgenode(3,ibe)
                else
                    jnode=edgenode(4,ibe)
                end if
                
                ! sub elm 1 connec
                elem%subcnc(1)%array(1)=edgenode(1,i1)
                elem%subcnc(1)%array(2)=edgenode(2,i1)
                elem%subcnc(1)%array(3)=jnode
                
                ! sub elm 2 connec
                elem%subcnc(2)%array(1)=edgenode(1,i2)
                elem%subcnc(2)%array(2)=edgenode(2,i2)
                elem%subcnc(2)%array(3)=jnode              
                
                ! sub elm 3 connec
                elem%subcnc(3)%array(1)=edgenode(1,i3)
                elem%subcnc(3)%array(2)=edgenode(2,i3)
                elem%subcnc(3)%array(3)=jnode                

          
          
         case (2) !- two edges cracked
          i=0
          ispartition=0
          do while (i<nedg.and.ispartition==0)
            i=i+1
            if((fedg(i)>0).and.
     &      (((i+2)<=nedg).and.(fedg(i+2)>0))) then ! opposing edges broken
                ispartition=1
                pstat=21._dp
                cncsub(1,1)=4 ! no. of nodes in bulk sub-elm 1
                cncsub(1,2)=4 ! no. of nodes in bulk sub-elm 2 
    !               cncsub(1,3)=-4 ! no. of nodes in coh sub-elm 3 
    !
                !-sub elm 1 connec
                if(i==1) then
                cncsub(1+1,1)=edg(1,4)
                cncsub(1+2,1)=edg(2,4)
                else
                cncsub(1+1,1)=edg(1,i-1)
                cncsub(1+2,1)=edg(2,i-1)
                end if
                cncsub(1+3,1)=actvnd(fedg(i),edg(3,i),edg(4,i),jelm)
                cncsub(1+4,1)=actvnd(fedg(i+2),edg(4,i+2),edg(3,i+2),
     &          jelm)
    !
                !-sub elm 2 connec
                cncsub(1+1,2)=edg(1,i+1)
                cncsub(1+2,2)=edg(2,i+1)
                cncsub(1+3,2)=actvnd(fedg(i+2),edg(3,i+2),edg(4,i+2),
     &           jelm)
                cncsub(1+4,2)=actvnd(fedg(i),edg(4,i),edg(3,i),jelm)
    !
                !-sub elm 3 connec (if exists)
                if(fedg(i)==4 .or. fedg(i+2)==4) then ! cohesive crack
                  pstat=31._dp
                  cncsub(1,3)=-4 ! cohesive sub-elm                 
                  cncsub(1+1,3)=actvnd(fedg(i),edg(4,i),edg(3,i),jelm)
                  cncsub(1+2,3)=actvnd(fedg(i+2),edg(3,i+2),edg(4,i+2)
     &             ,jelm)
                  cncsub(1+3,3)=actvnd(fedg(i+2),edg(4,i+2),edg(3,i+2)
     &             ,jelm)
                  cncsub(1+4,3)=actvnd(fedg(i),edg(3,i),edg(4,i),jelm)
                end if
             else if((fedg(i)>0).and.
     &      (((i+1)<=nedg).and.(fedg(i+1)>0))) then ! neighbouring edges broken
                ispartition=1
                pstat=41._dp
                cncsub(1,1)=3
                cncsub(1,2)=3
                cncsub(1,3)=3
                cncsub(1,4)=3
    !
                !-sub elm 1 connec
                cncsub(1+1,1)=edg(1,i+1)
                cncsub(1+2,1)=actvnd(fedg(i+1),edg(3,i+1),edg(4,i+1),
     &          jelm)
                cncsub(1+3,1)=actvnd(fedg(i),edg(4,i),edg(3,i),jelm)
    !
                !-sub elm 2 connec
                cncsub(1+1,2)=actvnd(fedg(i+1),edg(4,i+1),edg(3,i+1),
     &          jelm)
                cncsub(1+2,2)=edg(2,i+1)
                if(mod(i+2,nedg).ne.0) then 
                    cncsub(1+3,2)=edg(2,mod(i+2,nedg))
                else 
                    cncsub(1+3,2)=edg(2,nedg)
                end if
    !
                !-sub elm 3 connec
                cncsub(1+1,3)=actvnd(fedg(i+1),edg(4,i+1),edg(3,i+1),
     &          jelm)
                if(mod(i+2,nedg).ne.0) then 
                    cncsub(1+2,3)=edg(2,mod(i+2,nedg))
                else 
                    cncsub(1+2,3)=edg(2,nedg)
                end if
                cncsub(1+3,3)=actvnd(fedg(i),edg(3,i),edg(4,i),jelm)
    !
                !-sub elm 4 connec
                if(mod(i+3,nedg).ne.0) then
                    cncsub(1+1,4)=edg(1,mod(i+3,nedg)) 
                    cncsub(1+2,4)=edg(2,mod(i+3,nedg))
                else
                    cncsub(1+1,4)=edg(1,nedg) !-sub elm 4 connec
                    cncsub(1+2,4)=edg(2,nedg)
                end if
                cncsub(1+3,4)=actvnd(fedg(i),edg(3,i),edg(4,i),jelm)
                
                !-sub elm 5 connec (if exists)
                if(fedg(i)==4 .or. fedg(i+1)==4) then ! cohesive crack
                  pstat=51._dp
                  cncsub(1,5)=-4 ! cohesive sub-elm                 
                  cncsub(1+1,5)=actvnd(fedg(i),edg(4,i),edg(3,i),jelm)
                  cncsub(1+2,5)=actvnd(fedg(i+1),edg(3,i+1),edg(4,i+1)
     &             ,jelm)
                  cncsub(1+3,5)=actvnd(fedg(i+1),edg(4,i+1),edg(3,i+1)
     &             ,jelm)
                  cncsub(1+4,5)=actvnd(fedg(i),edg(3,i),edg(4,i),jelm)
                end if
                
             else if((fedg(i)>0).and.
     &      ((i==nedg).and.(fedg(1)>0))) then ! neighbouring edges broken (last edge & 1st edge)
                ispartition=1
                pstat=41._dp
                cncsub(1,1)=3
                cncsub(1,2)=3
                cncsub(1,3)=3
                cncsub(1,4)=3
                !-sub elm 1 connec
                cncsub(1+1,1)=edg(1,1) 
                cncsub(1+2,1)=actvnd(fedg(1),edg(3,1),edg(4,1),jelm)
                cncsub(1+3,1)=actvnd(fedg(i),edg(4,i),edg(3,i),jelm)
    !
                !-sub elm 2 connec
                cncsub(1+1,2)=actvnd(fedg(1),edg(4,1),edg(3,1),jelm)
                cncsub(1+2,2)=edg(2,1)
                if(mod(i+2,nedg).ne.0) then 
                    cncsub(1+3,2)=edg(2,mod(i+2,nedg))
                else 
                    cncsub(1+3,2)=edg(2,nedg)
                end if
    !
                !-sub elm 3 connec
                cncsub(1+1,3)=actvnd(fedg(1),edg(4,1),edg(3,1),jelm)
                if(mod(i+2,nedg).ne.0) then 
                    cncsub(1+2,3)=edg(2,mod(i+2,nedg))
                else 
                    cncsub(1+2,3)=edg(2,nedg)
                end if
                cncsub(1+3,3)=actvnd(fedg(i),edg(3,i),edg(4,i),jelm)
    !
                !-sub elm 4 connec
                if(mod(i+3,nedg).ne.0) then
                    cncsub(1+1,4)=edg(1,mod(i+3,nedg)) 
                    cncsub(1+2,4)=edg(2,mod(i+3,nedg))
                else
                    cncsub(1+1,4)=edg(1,nedg)
                    cncsub(1+2,4)=edg(2,nedg)
                end if
                cncsub(1+3,4)=actvnd(fedg(i),edg(3,i),edg(4,i),jelm)
                
                !-sub elm 5 connec (if exists)
                if(fedg(i)==4 .or. fedg(1)==4) then ! cohesive crack
                  pstat=51._dp
                  cncsub(1,5)=-4 ! cohesive sub-elm                 
                  cncsub(1+1,5)=actvnd(fedg(i),edg(4,i),edg(3,i),jelm)
                  cncsub(1+2,5)=actvnd(fedg(1),edg(3,1),edg(4,1)
     &             ,jelm)
                  cncsub(1+3,5)=actvnd(fedg(1),edg(4,1),edg(3,1)
     &             ,jelm)
                  cncsub(1+4,5)=actvnd(fedg(i),edg(3,i),edg(4,i),jelm)
                end if
                
            end if            
          end do

         case(3) !- three edges crack
           write(msg_file,*) 'three-edge precrack partition not yet supported!'
           nfedg=2
           goto 10
           
         case(4) !- four edges crack
           write(msg_file,*) 'four-edge precrack partition not yet supported!'
           nfedg=2
           goto 10
           
         case default
           write(msg_file,*) 'WARNING: precrack case selection default!'
           
        end select
                    


    end subroutine update_subcnc


  
  
  
  
  
  
  
  
  
  
end module xquad_element_module