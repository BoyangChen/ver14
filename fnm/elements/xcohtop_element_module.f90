module xcohtop_element_module
    use parameter_module
    use glb_clock_module
    use toolkit_module                  ! global tools for element integration
    use lib_edge_module                 ! global edge library
    use lib_node_module                 ! global node library
    use lib_mat_module                  ! global material library
    use sub3d_element_module
  
  
    implicit none
    private

    integer,parameter :: ndim=3, nndrl=8, nedge=4, nndfl=2*nedge, nnode=nndrl+nndfl, ndof=ndim*nnode
    ! Topology: nodes on each edge; 4 nodes per edge, 1-2 are end nodes, 3-4 are fl. nodes; assigned in lcl node numbers
    integer,parameter :: topo(4,nedge)=reshape([5,6,9,10,6,7,11,12,7,8,13,14,8,5,15,16],[4,nedge])
                                              
    ! element status variable values
    integer, parameter :: eltrans=1, elref=2, eltip=3, elwake=4, elfailm=5, elfailf=6
    
    ! edge status variable values
    integer, parameter :: egtrans=1, egref=2, egtip=3, wkcrack=3, cohcrack=4, strgcrack=5
    

    type, public :: xcohtop_element             ! breakable brick
        private
        
        integer :: curr_status=0        ! 0 means intact
        integer :: key=0 
        integer :: matkey=0
        
        integer :: nodecnc(nnode)=0     ! cnc to glb node arrays for accessing nodal variables (x, u, du, v, dof ...)
        integer :: edgecnc(nedge)=0     ! cnc to glb edge arrays for accessing edge variables (failure status)
        integer :: ifailedge(nedge)=0   ! indices of failed edges
        
        logical :: newpartition=.false. ! true when elem is changing partition, no failure should be considered then
        integer :: nstep=0, ninc=0      ! to store curr step and increment no.
        
        type(sub3d_element), allocatable :: subelem(:)
        type(int_alloc_array), allocatable :: subcnc(:)      ! sub_elem connec to parent elem nodes
        
        type(sdv_array), allocatable :: sdv(:)
        
    end type xcohtop_element
  
    interface empty
        module procedure empty_xcohtop_element
    end interface
  
    interface prepare
        module procedure prepare_xcohtop_element
    end interface
    
    !~interface precrack
    !~    module procedure precrack_xcohtop_element
    !~end interface
    
    interface integrate
        module procedure integrate_xcohtop_element
    end interface
    
    interface extract
        module procedure extract_xcohtop_element
    end interface




    public :: empty,prepare,integrate,extract



    contains




    ! empty a breakable quadrilateral
    subroutine empty_xcohtop_element(elem)
  
        type(xcohtop_element),intent(out) :: elem
        
        elem%curr_status=0
        elem%key=0 
        elem%matkey=0
        
        elem%nodecnc=0
        elem%edgecnc=0
        elem%ifailedge=0
        
        elem%newpartition=.false.
        elem%nstep=0
        elem%ninc=0
        
        if(allocated(elem%subelem)) deallocate(elem%subelem)
        if(allocated(elem%subcnc))  deallocate(elem%subcnc)
        if(allocated(elem%sdv)) deallocate(elem%sdv)

    end subroutine empty_xcohtop_element
  
  
  
  
  
    ! this subroutine is used to prepare the connectivity and material lib index of the element
    ! it is used in the initialize_lib_elem procedure in the lib_elem module
    subroutine prepare_xcohtop_element(elem,key,matkey,nodecnc,edgecnc)
    
        type(xcohtop_element),    intent(inout)   :: elem
        integer,                intent(in)      :: key
        integer,                intent(in)      :: matkey
        integer,                intent(in)      :: nodecnc(nnode)
        integer,                intent(in)      :: edgecnc(nedge)

        elem%key=key 
        elem%matkey=matkey
        elem%nodecnc=nodecnc
        elem%edgecnc=edgecnc
    
    end subroutine prepare_xcohtop_element
 



    ! this subroutine is used to update the ifailedge array of the element
    subroutine update_xcohtop_element(elem,ifailedge)
    
        type(xcohtop_element),    intent(inout)   :: elem
        integer,                  intent(in)      :: ifailedge(nedge)

        elem%ifailedge=ifailedge
    
    end subroutine update_xcohtop_element


   
    
    subroutine extract_xcohtop_element(elem,curr_status,key,matkey,nodecnc,edgecnc, &
    & ifailedge,newpartition,nstep,ninc,subelem,subcnc,sdv)
    
        type(xcohtop_element),                      intent(in)  :: elem
        integer,                        optional, intent(out) :: curr_status
        integer,                        optional, intent(out) :: key
        integer,                        optional, intent(out) :: matkey
        integer,            allocatable,optional, intent(out) :: nodecnc(:)
        integer,            allocatable,optional, intent(out) :: edgecnc(:)
        integer,            allocatable,optional, intent(out) :: ifailedge(:)
        logical,                        optional, intent(out) :: newpartition
        integer,                        optional, intent(out) :: nstep, ninc
        type(sub3d_element),allocatable,optional, intent(out) :: subelem(:)
        type(int_alloc_array),allocatable,optional,intent(out):: subcnc(:)
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
        
        if(present(newpartition)) newpartition=elem%newpartition
        
        if(present(nstep)) nstep=elem%nstep
        
        if(present(ninc)) ninc=elem%ninc
        
        if(present(subelem)) then
            if(allocated(elem%subelem)) then
                allocate(subelem(size(elem%subelem)))
                subelem=elem%subelem
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
    
    end subroutine extract_xcohtop_element






    subroutine integrate_xcohtop_element(elem, K_matrix, F_vector)
    
        type(xcohtop_element),intent(inout)       :: elem 
        real(kind=dp),allocatable,intent(out)   :: K_matrix(:,:), F_vector(:)
    
    
        ! local variables
        type(int_alloc_array), allocatable  :: subglbcnc(:)     ! glb cnc of sub element, used when elem is intact
        real(kind=dp),allocatable           :: Ki(:,:), Fi(:)   ! sub_elem K matrix and F vector
        
        integer :: i,j,l, elstat, subelstat
        integer, allocatable :: dofcnc(:)
        
        character(len=eltypelength) ::  subeltype
        
        logical :: nofailure
        
        ! - glb clock step and increment no. extracted from glb clock module
        integer :: curr_step, curr_inc
        logical :: last_converged               ! true if last iteration has converged: a new increment/step has started
    
    
        ! initialize K & F
        allocate(K_matrix(ndof,ndof),F_vector(ndof))
        K_matrix=zero; F_vector=zero
        
        ! initialize local variables
        i=0; j=0; l=0
        elstat=0; subelstat=0; subeltype=''
        nofailure=.false.
        curr_step=0; curr_inc=0
        last_converged=.false.


        ! - extract curr step and inc values from glb clock module
        call extract_glb_clock(kstep=curr_step,kinc=curr_inc)
        
        ! - check if last iteration has converged, and update the current step & increment no.
        if(elem%nstep.ne.curr_step .or. elem%ninc.ne.curr_inc) then
            last_converged=.true.
            elem%nstep = curr_step
            elem%ninc = curr_inc
            elem%newpartition=.false.   ! last partition has converged; newpartition is false
        end if

        
        

        !---------------------------------------------------------------------!
        !       update elem partition using edge status variable
        !---------------------------------------------------------------------!
     
        ! if elem is not yet failed, check elem edge status variables and update elem status and sub elem cnc

        ! store current status value
        elstat=elem%curr_status  
        call edge_status_partition(elem) 

        ! if elem is still intact after checking edge status (no broken edges), assign 1 coh3d8 subelem if not yet done
        if(.not.allocated(elem%subelem)) then 
            allocate(elem%subelem(1))
            allocate(elem%subcnc(1))
            allocate(elem%subcnc(1)%array(nndrl))   ! coh3d8 elem
            allocate(subglbcnc(1))
            allocate(subglbcnc(1)%array(nndrl))
            ! sub elm 1 connec
            elem%subcnc(1)%array=[(i, i=1,nndrl)]
            subglbcnc(1)%array(:)=elem%nodecnc(elem%subcnc(1)%array(:))
            ! create sub elements
            call prepare(elem%subelem(1),eltype='coh3d8', matkey=elem%matkey, &
            & glbcnc=subglbcnc(1)%array)
        end if
            

        !---------------------------------------------------------------------!
        !       integrate and assemble sub element system arrays
        !---------------------------------------------------------------------!        

        
        ! empty K and F for reuse
        K_matrix=zero; F_vector=zero  
     
        ! integrate sub elements and assemble into global matrix
        ! if elem partition just changed in this iteration, no failure modelling at this iteration
        do i=1, size(elem%subelem)
            call integrate(elem%subelem(i),Ki,Fi,nofailure)
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

            
        
        !---------------------------------------------------------------------!
        !               deallocate local arrays 
        !---------------------------------------------------------------------!
        if(allocated(Ki)) deallocate(Ki)
        if(allocated(Fi)) deallocate(Fi)
        if(allocated(subglbcnc)) deallocate(subglbcnc)
        if(allocated(dofcnc)) deallocate(dofcnc)
    
 
    end subroutine integrate_xcohtop_element
    
    
    
    
    
    
    
    subroutine edge_status_partition(elem)

    
    ! passed-in variables
    type(xcohtop_element), intent(inout) :: elem


    ! extracted variables, from glb libraries
    integer :: edgstat(nedge)               ! status variable array of element edges
    type(real_alloc_array) :: coord(nnode)  ! nodal coord arrays to store the coords of elem nodes extracted from glb node lib
    
    
    
    ! local variable
    
    integer :: nfailedge        ! no. of failed edges in the element
    integer :: ifedg(nedge)     ! index of failed edges in the element
    integer :: elstat           ! local copy of elem curr status
    integer :: jbe1,jbe2,jbe3,jbe4, jnode ! indices of broken edges, and a variable to hold a glb node index
    integer :: iscross          ! indicator of line intersection; >0 if two lines intersect
    integer :: i, j, l, k       ! counters
      
    real(dp) :: xp1, yp1, xp2, yp2      ! (x,y) of point 1 and point 2 on the crack line
    real(dp) :: x1, y1, z1, x2, y2, z2  ! (x,y) of node 1 and node 2 of an element edge
    real(dp) :: xct, yct, zct           ! (x,y) of a crack tip on an edge, i.e., intersection of crack line & edge
    real(dp) :: detlc,a1,b1,a2,b2,xmid,ymid

    
    ! --------------------------------------------------------------------!
    !       *** workings of edgstat, nfailedge, ifedg ***
    !
    !       e.g.: element edge 1 and 3 are broken, then:
    !
    !           - nfailedge=2
    !           - edgstat(1)>0; edgstat(2)=0; edgstat(3)>0; edgstat(4)=0
    !           - ifedg(1)=1; ifedg(2)=3; ifedg(3:)=0
    !
    ! --------------------------------------------------------------------!
    
    
        ! initialize local variables
        
        edgstat=0; 
        nfailedge=0; ifedg=0; elstat=0
        jbe1=0; jbe2=0; jbe3=0; jbe4=0; jnode=0
        iscross=0
        i=0; j=0; l=0; k=0
        
        xp1=zero; yp1=zero
        xp2=zero; yp2=zero

        x1=zero; y1=zero; z1=zero
        x2=zero; y2=zero; z2=zero
        
        xct=zero; yct=zero; zct=zero

        detlc=zero; a1=zero; b1=zero; a2=zero; b2=zero; xmid=zero; ymid=zero




!-----------------------------------------------------------------------!
!               EXTRACTION INTERFACE
!           extract variables from global libraries
!-----------------------------------------------------------------------!
        ! extract elem status variable
        elstat=elem%curr_status

        ! extract edge status variables from glb edge library
        edgstat(:)=lib_edge(elem%edgecnc(:))
        
        ! extract nodal coords from glb node library
        do i=1, nnode
            call extract(lib_node(elem%nodecnc(i)),x=coord(i)%array)
        end do


        ! extract elem failed edges' indices (this info is passed from adj. ply elem)
        ifedg=elem%ifailedge

!-----------------------------------------------------------------------!
!       procedure calculations (pure)
!-----------------------------------------------------------------------!
    
!       find and store the broken edges' variables
        do i=1,size(ifedg)
            if(ifedg(i)>0) nfailedge=nfailedge+1
        end do
    
        
        if(nfailedge==0) then
        ! remains intact, do nothing
            continue
        else if(nfailedge==1) then 
        ! adj. ply elem is transition partition
        
            if(edgstat(ifedg(1))==egtrans) then
              ! edge marks the refinement end and the trans elem start 
              ! elem is a trans elem, only this edge needs to be partitioned
                elstat=eltrans
                
            else
              ! ifailedge is not correct
              write(msg_file,*)'wrong edge status for nfailedge=1 in xcohtop'
              call exit_function
        
            endif
            
        else if(nfailedge==2) then
        ! adj. ply elem could be cracked, wake, tip, refinement elem
        ! only update the elstat, not the edge status, nor the crack tip coords
        
            !**** the partitioning of xcohtop elem is entirely based on the bottom four edges ****         
            
            jbe1=ifedg(1)
            jbe2=ifedg(2)
            
            if(edgstat(jbe1)<=egref .and. edgstat(jbe2)<=egref) then
            ! refinement elem
                elstat=elref
            else if((edgstat(jbe1)<=egtip .and. edgstat(jbe2)==egtip).or. &
            & (edgstat(jbe2)<=egtip .and. edgstat(jbe1)==egtip)) then
            ! tip elem
                elstat=eltip
            else if((edgstat(jbe1)<cohcrack .and. edgstat(jbe2)>=cohcrack).or. &
            & (edgstat(jbe2)<cohcrack .and. edgstat(jbe1)>=cohcrack)) then
            ! wake elem, cohesive/stress-free crack
                elstat=elwake
            else if(edgstat(jbe1)>=cohcrack .and. edgstat(jbe2)>=cohcrack) then
            ! cracked elem, cohesive/stress-free crack
                elstat=elfailm
            else ! unknown combination
                write(msg_file,*)'unknown combination of 2 edge status in xcohtop!'
                call exit_function
            end if
            
        else
            write(msg_file,*)'unsupported nfailedge value for edge and el stat update in xcohtop edge stat partition!'
            call exit_function 
        end if     

!-----------------------------------------------------------------------!
!                   UPDATE INTERFACE
!               update global libraries
!-----------------------------------------------------------------------!
            
!       update element curr_status and sub-element cnc matrices

        if(elstat>elem%curr_status) then

            elem%curr_status=elstat                    

            call update_subcnc(elem,edgstat,ifedg,nfailedge)
        
        end if





!       deallocate local dynamic arrays

    
    
    end subroutine edge_status_partition
    
  



       
    subroutine update_subcnc(elem,edgstat,ifedg,nfailedge)
    
    ! passed-in variables
    type(xcohtop_element),    intent(inout)   :: elem
    integer,                intent(in)      :: edgstat(:), ifedg(:), nfailedge



    ! local variables
    type(int_alloc_array), allocatable :: subglbcnc(:)  ! glb cnc of sub elements
    integer :: i, j, l                                  ! counters
    integer :: ibe, ibe1, ibe2                          ! indices of broken edges
    integer :: e1,e2,e3,e4                              ! edge indices, used for partitioning element
    integer :: nsub, nbulk                              ! no. of sub elements, and no. of bulk partitions
    integer :: jnode, jnode1, jnode2, jnode3, jnode4    ! node index variables
    logical :: iscoh



!       initialize local variables

        i=0; j=0; l=0
        e1=0; e2=0; e3=0; e4=0
        ibe=0; ibe1=0; ibe2=0
        nsub=0; nbulk=0
        jnode=0; jnode1=0; jnode2=0; jnode3=0; jnode4=0
        iscoh=.false.
        


10      select case (nfailedge)
        case (0) !- no cracked edge, do nothing
            continue
            
            
        case (1) !- one edge cracked, trans partition
            ! find the index of the broken edge
            ibe=ifedg(1)

            ! ibe1 must be between 1 to 4
            if(ibe1<1 .or. ibe1>4) then
                write(msg_file,*) 'something wrong in xcohtop update subcnc case nfailedge=1'
                call exit_function
            end if

            ! verify its status variable value
            if(edgstat(ibe)/=egtrans) then
                write(msg_file,*)'transition partition only accepts edgstat=egtrans!'
                call exit_function
            end if
            
            ! allocate sub element arrays; in this case, 3 wedge elements
            nsub=3
            if(allocated(elem%subelem)) deallocate(elem%subelem)
            if(allocated(elem%subcnc)) deallocate(elem%subcnc)
            if(allocated(subglbcnc)) deallocate(subglbcnc)
            allocate(elem%subelem(nsub))
            allocate(elem%subcnc(nsub))
            allocate(subglbcnc(nsub))
            do j=1, nsub
                allocate(elem%subcnc(j)%array(6))
                allocate(subglbcnc(j)%array(6))
                elem%subcnc(j)%array=0
                subglbcnc(j)%array=0
            end do             
            
            ! find the neighbouring edges of this broken edge, in counter-clockwise direction
            select case(ibe)
                case (1)
                    e1=2;e2=3;e3=4
                case (2)
                    e1=3;e2=4;e3=1
                case (3)
                    e1=4;e2=1;e3=2
                case (4)
                    e1=1;e2=2;e3=3
                case default
                    write(msg_file,*)'wrong broken edge in tip partition, subcnc'
                    call exit_function
            end select
            
            ! find the smaller glb node on the broken edge
            if(elem%nodecnc(topo(3,ibe))<elem%nodecnc(topo(4,ibe))) then
                jnode=topo(3,ibe)
            else
                jnode=topo(4,ibe)
            end if
            
            ! sub elm 1 connec
            elem%subcnc(1)%array(1)=topo(1,e1)
            elem%subcnc(1)%array(2)=topo(2,e1)
            elem%subcnc(1)%array(3)=jnode
            
            elem%subcnc(1)%array(4:5)=elem%subcnc(1)%array(1:2)+nndrl/2 ! upper surf nodes
            elem%subcnc(1)%array(6)=elem%subcnc(1)%array(3)+nndfl/2
            
            subglbcnc(1)%array(:)=elem%nodecnc(elem%subcnc(1)%array(:))
            
            ! sub elm 2 connec
            elem%subcnc(2)%array(1)=topo(1,e2)
            elem%subcnc(2)%array(2)=topo(2,e2)
            elem%subcnc(2)%array(3)=jnode 

            elem%subcnc(2)%array(4:5)=elem%subcnc(2)%array(1:2)+nndrl/2 ! upper surf nodes
            elem%subcnc(2)%array(6)=elem%subcnc(2)%array(3)+nndfl/2 

            subglbcnc(2)%array(:)=elem%nodecnc(elem%subcnc(2)%array(:))
            
            ! sub elm 3 connec
            elem%subcnc(3)%array(1)=topo(1,e3)
            elem%subcnc(3)%array(2)=topo(2,e3)
            elem%subcnc(3)%array(3)=jnode  
            
            elem%subcnc(3)%array(4:5)=elem%subcnc(3)%array(1:2)+nndrl/2 ! upper surf nodes
            elem%subcnc(3)%array(6)=elem%subcnc(3)%array(3)+nndfl/2

            subglbcnc(3)%array(:)=elem%nodecnc(elem%subcnc(3)%array(:))

            ! create sub elements
            call prepare(elem%subelem(1),eltype='wedge', matkey=elem%bulkmat, plyangle=elem%plyangle, glbcnc=subglbcnc(1)%array)
            call prepare(elem%subelem(2),eltype='wedge', matkey=elem%bulkmat, plyangle=elem%plyangle, glbcnc=subglbcnc(2)%array)
            call prepare(elem%subelem(3),eltype='wedge', matkey=elem%bulkmat, plyangle=elem%plyangle, glbcnc=subglbcnc(3)%array)
         

 
        case (2) !- two edges cracked
         
            ibe1=min(ifedg(1),ifedg(2))   ! local edge index of 1st broken edge
            ibe2=max(ifedg(1),ifedg(2)) 
            
            
            ! ibe1 must be between 1 to 3, and ibe2 between 2 to 4, with ibe2 > ibe1
            if(ibe1<1 .or. ibe1>3 .or. ibe2<2 .or. ibe2>4 .or. ibe2<=ibe1) then
                write(msg_file,*) 'something wrong in xcohtop update subcnc case nfailedge=2'
                call exit_function
            end if

            
            ! determine partition based on the indices of the two broken edges
            !   partition: no. of bulk sub domains
            !   e1 - e4: re-index edges to facilitate partitioning domain
            select case(ibe1)
                case(1)
                    select case(ibe2)
                        case(2)
                            nbulk=4
                            e1=1; e2=2; e3=3; e4=4
                        case(3)
                            nbulk=2
                            e1=1; e2=2; e3=3; e4=4
                        case(4)
                            nbulk=4
                            e1=4; e2=1; e3=2; e4=3
                        case default
                            write(msg_file,*)'wrong 2nd broken edge in update subcnc xcohtop'
                            call exit_function
                    end select
                case(2)
                    select case(ibe2)
                        case(3)
                            nbulk=4
                            e1=2; e2=3; e3=4; e4=1
                        case(4)
                            nbulk=2
                            e1=2; e2=3; e3=4; e4=1
                        case default
                            write(msg_file,*)'wrong 2nd broken edge in update subcnc xcohtop'
                            call exit_function
                    end select
                case(3)
                    if(ibe2==4) then
                        nbulk=4
                        e1=3; e2=4; e3=1; e4=2
                    else
                        write(msg_file,*)'wrong 2nd broken edge in update subcnc xcohtop'
                        call exit_function                    
                    end if    
                case default
                    write(msg_file,*)'wrong broken edge in update subcnc xcohtop'
                    call exit_function
            end select
            
            
            select case(nbulk)
                case(2)
                ! two brick bulk subdomains

                    nsub=2  ! only two brick

                    if(allocated(elem%subelem)) deallocate(elem%subelem)
                    if(allocated(elem%subcnc)) deallocate(elem%subcnc)
                    if(allocated(subglbcnc)) deallocate(subglbcnc)
                    allocate(elem%subelem(nsub))
                    allocate(elem%subcnc(nsub))
                    allocate(subglbcnc(nsub))
                    do j=1, nsub
                        allocate(elem%subcnc(j)%array(8))
                        allocate(subglbcnc(j)%array(8))
                        elem%subcnc(j)%array=0
                        subglbcnc(j)%array=0
                    end do
                    
                    ! find the smaller glb fl. node on the broken edge
                    if(elem%nodecnc(topo(3,e1))<elem%nodecnc(topo(4,e1))) then
                        jnode1=topo(3,e1)
                    else
                        jnode1=topo(4,e1)
                    end if
                    
                    ! find the smaller glb fl. node on the broken edge
                    if(elem%nodecnc(topo(3,e3))<elem%nodecnc(topo(4,e3))) then
                        jnode3=topo(3,e3)
                    else
                        jnode3=topo(4,e3)
                    end if
                    
                    ! sub elm 1 connec
                    elem%subcnc(1)%array(1)=topo(1,e1)
                    elem%subcnc(1)%array(2)=topo(3,e1); if(edgstat(e1)<cohcrack) elem%subcnc(1)%array(2)=jnode1
                    elem%subcnc(1)%array(3)=topo(4,e3); if(edgstat(e3)<cohcrack) elem%subcnc(1)%array(3)=jnode3
                    elem%subcnc(1)%array(4)=topo(2,e3)
                    
                    elem%subcnc(1)%array(5)=elem%subcnc(1)%array(1)+nndrl/2
                    elem%subcnc(1)%array(6)=elem%subcnc(1)%array(2)+nndfl/2
                    elem%subcnc(1)%array(7)=elem%subcnc(1)%array(3)+nndfl/2
                    elem%subcnc(1)%array(8)=elem%subcnc(1)%array(4)+nndrl/2
                    
                    subglbcnc(1)%array(:)=elem%nodecnc(elem%subcnc(1)%array(:))
                    
                    ! sub elm 2 connec
                    elem%subcnc(2)%array(1)=topo(4,e1); if(edgstat(e1)<cohcrack) elem%subcnc(2)%array(1)=jnode1
                    elem%subcnc(2)%array(2)=topo(2,e1)
                    elem%subcnc(2)%array(3)=topo(1,e3)
                    elem%subcnc(2)%array(4)=topo(3,e3); if(edgstat(e3)<cohcrack) elem%subcnc(2)%array(4)=jnode3
                    
                    elem%subcnc(2)%array(5)=elem%subcnc(2)%array(1)+nndfl/2
                    elem%subcnc(2)%array(6)=elem%subcnc(2)%array(2)+nndrl/2
                    elem%subcnc(2)%array(7)=elem%subcnc(2)%array(3)+nndrl/2
                    elem%subcnc(2)%array(8)=elem%subcnc(2)%array(4)+nndfl/2

                    subglbcnc(2)%array(:)=elem%nodecnc(elem%subcnc(2)%array(:))
                    
                    ! create sub bulk elements
                    
                    call prepare(elem%subelem(1),eltype='brick',matkey=elem%bulkmat,plyangle=elem%plyangle,&
                    &glbcnc=subglbcnc(1)%array)
                    call prepare(elem%subelem(2),eltype='brick',matkey=elem%bulkmat,plyangle=elem%plyangle,&
                    &glbcnc=subglbcnc(2)%array)
    
                    
                    !call prepare(elem%subelem(3),eltype='coh3d8',matkey=elem%cohmat,glbcnc=subglbcnc(3)%array)
                    
                    
                    
                case(4)
                ! four triangular bulk subdomains

                    nsub=4

                    if(allocated(elem%subelem)) deallocate(elem%subelem)
                    if(allocated(elem%subcnc)) deallocate(elem%subcnc)
                    if(allocated(subglbcnc)) deallocate(subglbcnc)
                    allocate(elem%subelem(nsub))
                    allocate(elem%subcnc(nsub))
                    allocate(subglbcnc(nsub))
                    do j=1, 4   ! bulk elem cnc
                        allocate(elem%subcnc(j)%array(6))
                        allocate(subglbcnc(j)%array(6))
                        elem%subcnc(j)%array=0
                        subglbcnc(j)%array=0
                    end do

                    
                    ! find the smaller glb fl. node on the broken edge
                    if(elem%nodecnc(topo(3,e1))<elem%nodecnc(topo(4,e1))) then
                        jnode1=topo(3,e1)
                    else
                        jnode1=topo(4,e1)
                    end if
                    
                    ! find the smaller glb fl. node on the broken edge
                    if(elem%nodecnc(topo(3,e2))<elem%nodecnc(topo(4,e2))) then
                        jnode2=topo(3,e2)
                    else
                        jnode2=topo(4,e2)
                    end if
                    
                    ! sub elm 1 connec
                    elem%subcnc(1)%array(1)=topo(4,e1); if(edgstat(e1)<cohcrack) elem%subcnc(1)%array(1)=jnode1
                    elem%subcnc(1)%array(2)=topo(1,e2)
                    elem%subcnc(1)%array(3)=topo(3,e2); if(edgstat(e2)<cohcrack) elem%subcnc(1)%array(3)=jnode2

                    elem%subcnc(1)%array(4)=elem%subcnc(1)%array(1)+nndfl/2 ! upper surf nodes
                    elem%subcnc(1)%array(5)=elem%subcnc(1)%array(2)+nndrl/2
                    elem%subcnc(1)%array(6)=elem%subcnc(1)%array(3)+nndfl/2
                    
                    subglbcnc(1)%array(:)=elem%nodecnc(elem%subcnc(1)%array(:))
                    
                    ! sub elm 2 connec
                    elem%subcnc(2)%array(1)=topo(4,e2); if(edgstat(e2)<cohcrack) elem%subcnc(2)%array(1)=jnode2
                    elem%subcnc(2)%array(2)=topo(1,e3)
                    elem%subcnc(2)%array(3)=topo(2,e3)
                    
                    elem%subcnc(2)%array(4)=elem%subcnc(2)%array(1)+nndfl/2 ! upper surf nodes
                    elem%subcnc(2)%array(5)=elem%subcnc(2)%array(2)+nndrl/2
                    elem%subcnc(2)%array(6)=elem%subcnc(2)%array(3)+nndrl/2
                    
                    subglbcnc(2)%array(:)=elem%nodecnc(elem%subcnc(2)%array(:))
                    
                    ! sub elm 3 connec
                    elem%subcnc(3)%array(1)=topo(1,e4)
                    elem%subcnc(3)%array(2)=topo(2,e4)
                    elem%subcnc(3)%array(3)=topo(3,e1); if(edgstat(e1)<cohcrack) elem%subcnc(3)%array(3)=jnode1
                    
                    elem%subcnc(3)%array(4)=elem%subcnc(3)%array(1)+nndrl/2 ! upper surf nodes
                    elem%subcnc(3)%array(5)=elem%subcnc(3)%array(2)+nndrl/2
                    elem%subcnc(3)%array(6)=elem%subcnc(3)%array(3)+nndfl/2
                    
                    subglbcnc(3)%array(:)=elem%nodecnc(elem%subcnc(3)%array(:))
                    
                    ! sub elm 4 connec
                    elem%subcnc(4)%array(1)=topo(3,e1); if(edgstat(e1)<cohcrack) elem%subcnc(4)%array(1)=jnode1
                    elem%subcnc(4)%array(2)=topo(4,e2); if(edgstat(e2)<cohcrack) elem%subcnc(4)%array(2)=jnode2
                    elem%subcnc(4)%array(3)=topo(2,e3)

                    elem%subcnc(4)%array(4)=elem%subcnc(4)%array(1)+nndfl/2 ! upper surf nodes
                    elem%subcnc(4)%array(5)=elem%subcnc(4)%array(2)+nndfl/2
                    elem%subcnc(4)%array(6)=elem%subcnc(4)%array(3)+nndrl/2
                    
                    subglbcnc(4)%array(:)=elem%nodecnc(elem%subcnc(4)%array(:))
                    
                    ! create sub bulk elements
                    call prepare(elem%subelem(1),eltype='wedge',matkey=elem%bulkmat,&
                    &plyangle=elem%plyangle,glbcnc=subglbcnc(1)%array)
                    call prepare(elem%subelem(2),eltype='wedge',matkey=elem%bulkmat,&
                    &plyangle=elem%plyangle,glbcnc=subglbcnc(2)%array)
                    call prepare(elem%subelem(3),eltype='wedge',matkey=elem%bulkmat,&
                    &plyangle=elem%plyangle,glbcnc=subglbcnc(3)%array)
                    call prepare(elem%subelem(4),eltype='wedge',matkey=elem%bulkmat,&
                    &plyangle=elem%plyangle,glbcnc=subglbcnc(4)%array)
                    
                    
                    !call prepare(elem%subelem(5),eltype='coh3d8', matkey=elem%cohmat, glbcnc=subglbcnc(5)%array)
                    
                    
                case default
                    write(msg_file,*)'wrong nbulk in update subcnc xcohtop'
                    call exit_function
            end select

          
!        case(6)
        ! do not update partition
            

!        case(8)


           
        case default
           write(msg_file,*) 'WARNING: xcohtop update subcnc case selection default!'
           
        end select
        
        
        ! deallocate local array
        
        if(allocated(subglbcnc)) deallocate(subglbcnc)


    end subroutine update_subcnc


  
  
  
  
  
  
  
  
  
  
end module xcohtop_element_module
