module xquad_element_module
    use parameter_module
    use toolkit_module                  ! global tools for element integration
    use lib_edge_module                 ! global edge library
    use lib_node_module                 ! global node library
    use lib_mat_module                  ! global material library
    use sub2d_element_module
  
  
    implicit none
    private

    integer,parameter :: ndim=2, nndrl=4, nedge=4, nndfl=2*nedge, nnode=nndrl+nndfl, ndof=ndim*nnode
    ! Topology: nodes on each edge; 4 nodes per edge, 1-2 are end nodes, 3-4 are fl. nodes; assigned in lcl node numbers
    integer,parameter :: topo(4,nedge)=reshape([1,2,5,6,2,3,7,8,3,4,9,10,4,1,11,12],[4,nedge]) 

    type, public :: xquad_element             ! breakable quadrilateral
        private
        
        integer :: curr_status=0        ! 0 means intact
        integer :: key=0 
        integer :: bulkmat=0
        integer :: cohmat=0
        real(dp):: theta=zero           ! fibre orientation for lamina
        
        integer :: nodecnc(nnode)=0     ! cnc to glb node arrays for accessing nodal variables (x, u, du, v, dof ...)
        integer :: edgecnc(nedge)=0     ! cnc to glb edge arrays for accessing edge variables (failure status)
        
        type(sub2d_element), allocatable :: subelem(:)
        type(int_alloc_array), allocatable :: subcnc(:)      ! sub_elem connec to parent elem nodes
        
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
        elem%bulkmat=0
        elem%cohmat=0
        elem%theta=zero
        
        elem%nodecnc(nnode)=0
        elem%edgecnc(nedge)=0
        
        if(allocated(elem%subelem)) deallocate(elem%subelem)
        if(allocated(elem%subcnc))  deallocate(elem%subcnc)

    end subroutine empty_xquad_element
  
  
  
    ! this subroutine is used to prepare the connectivity and material lib index of the element
    ! it is used in the initialize_lib_elem procedure in the lib_elem module
    subroutine prepare_xquad_element(elem,key,bulkmat,cohmat,theta,nodecnc,edgecnc)
    
        type(xquad_element),    intent(inout)   :: elem
        integer,                intent(in)      :: key
        integer,                intent(in)      :: matkey
        real(dp),               intent(in)      :: theta
        integer,                intent(in)      :: nodecnc(nnode)
        integer,                intent(in)      :: edgecnc(nedge)

        elem%key=key 
        elem%bulkmat=bulkmat
        elem%cohmat=cohmat
        elem%theta=theta
        elem%nodecnc=nodecnc
        elem%edgecnc=edgecnc
    
    end subroutine prepare_xquad_element
    
    
    subroutine extract_xquad_element(elem,curr_status,key,bulkmat,cohmat,theta,nodecnc,edgecnc,subelem,subcnc)
    
        type(xquad_element),                      intent(in)  :: elem
        integer,                        optional, intent(out) :: curr_status
        integer,                        optional, intent(out) :: key
        integer,                        optional, intent(out) :: bulkmat
        integer,                        optional, intent(out) :: cohmat
        real(dp),                       optional, intent(out) :: theta
        integer,            allocatable,optional, intent(out) :: nodecnc(:)
        integer,            allocatable,optional, intent(out) :: edgecnc(:)
        type(sub2d_element),allocatable,optional, intent(out) :: subelem(:)
        type(int_alloc_array),allocatable,optional,intent(out):: subcnc(:)

        if(present(curr_status)) curr_status=elem%curr_status
        if(present(key)) key=elem%key 
        if(present(bulkmat)) bulkmat=elem%bulkmat
        if(present(cohmat)) cohmat=elem%cohmat
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
        
        if(present(subcnc)) then
            if(allocated(elem%subcnc)) then
                allocate(subcnc(size(elem%subcnc)))
                subcnc=elem%subcnc
            end if
        end if
    
    end subroutine extract_xquad_element






    subroutine integrate_xquad_element(elem, K_matrix, F_vector)
    
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
            call edge_status_partition(elem)
            
            if(elem%curr_status==0) then 
            ! elem still intact, no broken edges
                call failure_criterion_partition(elem)
            end if
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

    
    ! passed-in variables
    type(xquad_element), intent(inout) :: elem


    ! extracted variables, from glb libraries
    integer :: edgstat(nedge)               ! status variable array of element edges
    type(real_alloc_array) :: coord(nnode)  ! nodal coord arrays to store the coords of elem nodes extracted from glb node lib
    
    
    
    ! local variable
    
    integer :: nfailedge        ! no. of failed edges in the element
    integer :: ifedg(nedge)     ! index of failed edges in the element
    integer :: elstat           ! local copy of elem curr status
    integer :: jbe1,jbe2, jnode ! indices of 2 broken edges, and a variable to hold a glb node index
    integer :: iscross          ! indicator of line intersection; >0 if two lines intersect
    integer :: i, j, l, k       ! counters
      
    real(dp) :: xp1, yp1, xp2, yp2      ! (x,y) of point 1 and point 2 on the crack line
    real(dp) :: x1, y1, x2, y2          ! (x,y) of node 1 and node 2 of an element edge
    real(dp) :: xct, yct                ! (x,y) of a crack tip on an edge, i.e., intersection of crack line & edge
    
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
        jbe1=0; jbe2=0; jnode=0
        iscross=0
        i=0; j=0; l=0; k=0
        
        xp1=zero; yp1=zero
        xp2=zero; yp2=zero

        x1=zero; y1=zero
        x2=zero; y2=zero
        
        xct=zero; yct=zero


!-----------------------------------------------------------------------!
!               EXTRACTION INTERFACE
!           extract variables from global libraries
!-----------------------------------------------------------------------!
        ! extract edge status variables from glb edge library
        edgstat(:)=lib_edge(elem%edgecnc(:))
        
        ! extract nodal coords from glb node library
        do i=1, nnode
            call extract(lib_node(elem%nodecnc(i)),x=coord(i)%array)
        end do



!-----------------------------------------------------------------------!
!       procedure calculations (pure)
!-----------------------------------------------------------------------!
    
!       find and store the broken edges' variables
        do i=1,nedge
            if(edgstat(i)/=0) then
                nfailedge=nfailedge+1   ! update total no. of damaged edges
                ifedg(nfailedge)=i  ! update the indices of damaged edges
            end if
        end do
     

!       calculate elstat value from edge status variables

10      if(nfailedge==0) then
            ! no edge failed/damaged, do nothing
            continue
            
        else if(nfailedge==1) then 
        ! could be 1st time wake elm, tip elm, ref elm and trans elm
        ! update the edge status, crack tip coords and elstat accordingly
        
            if(edgstat(ifedg(1))==1) then
              ! edge marks the refinement end and the trans elem start 
              ! elem is a trans elem, only this edge needs to be partitioned
                elstat=1
                
            else
              ! another edge must be partitioned to form a ref/tip/wake elem
              ! find the other edge to be partitioned
                
                ! first, find the index of the broken edge
                jbe1=ifedg(1)
                
                ! find the first (or the second also can) fl. node on this edge 
                jnode=topo(3,jbe1)
                
                ! store x,y values of this node in xp1, yp1 (legacy format)
                xp1=coord(jnode)%array(1)
                yp1=coord(jnode)%array(2)

                ! from theta (local fibre dir.), calculate another point on the crack line (could be any point along the line)
                xp2=xp1+cos(elem%theta/halfcirc*pi)
                yp2=yp1+sin(elem%theta/halfcirc*pi)
                
                ! next, find the other edge crossed by the crack line
                do i=1,nedge
                    if (i==jbe1) cycle ! the already broken edge, go to next edge
                    
                    ! extract end node 1 coords of edge i
                    jnode=topo(1,i)  
                    x1=coord(jnode)%array(1)
                    y1=coord(jnode)%array(2)
                    
                    ! extract end node 2 coords of edge i
                    jnode=topo(2,i)
                    x2=coord(jnode)%array(1)
                    y2=coord(jnode)%array(2)

                    ! zero cross status and intersection coords for reuse
                    iscross=0
                    xct=zero
                    yct=zero
                    
                    ! check intersection of the crack line and the edge
                    call klinecross(x1,y1,x2,y2,xp1,yp1,xp2,yp2,iscross,xct,yct)
                    if (iscross>0) exit ! found the edge, no need to proceed
                end do
                
                ! update nfailedge & ifedg
                nfailedge=2     ! no. of broken edge is now 2
                ifedg(2)=i  ! index of the 2nd broken edge is i
                
                ! store it in jbe2 (safer)
                jbe2=i 
                
                ! update the two fl. node coords on this edge
                jnode=topo(3,jbe2)
                coord(jnode)%array=[xct,yct]
                jnode=topo(4,jbe2)               
                coord(jnode)%array=[xct,yct]
                
                ! update edge status variables
                if(edgstat(jbe1)==2) then ! tip elem end, refinement elem. start (1st time)
                    elstat=2               
                    ! change 2nd broken edge status to 1 (trans elem start)
                    edgstat(jbe2)=1
             
                else if(edgstat(jbe1)==3) then ! wake elem end, tip elem start (1st time)
                    elstat=3
                    ! change 2nd broken edge status to 2 (refinement start)
                    edgstat(jbe2)=2
               
                else if(edgstat(jbe1)>=4) then ! wake elem start (1st time)
                    elstat=4 !wake elem               
                    ! change 2nd broken edge status to 3 (tip elem start)
                    edgstat(jbe2)=3
                    
                else ! unknown edge status
                    write(msg_file,*)'unknown edge status!'
                    call exit_function
                end if        
            endif
                
        else if(nfailedge==2) then 
        ! could be cracked, wake, tip, refinement elem
        ! only update the elstat, not the edge status, nor the crack tip coords
            jbe1=ifedg(1)
            jbe2=ifedg(2)
            if(edgstat(jbe1)<=2 .and. edgstat(jbe2)<=2) then
            ! refinement elem
                elstat=2         
            else if((edgstat(jbe1)<=3 .and. edgstat(jbe2)==3).or.(edgstat(jbe2)<=3 .and. edgstat(jbe1)==3)) then
            ! tip elem
                elstat=3
            else if((edgstat(jbe1)<4 .and. edgstat(jbe2)>=4).or.(edgstat(jbe2)<4 .and. edgstat(jbe1)>=4)) then
            ! wake elem, cohesive/stress-free crack
                elstat=4
            else if(edgstat(jbe1)>=4 .and. edgstat(jbe2)>=4) then
            ! cracked elem, cohesive/stress-free crack
                elstat=5        
            else ! unknown combination
                write(msg_file,*)'unknown combination of 2 edge status!'
                call exit_function
            end if
          
        else if(nfailedge>2) then
        ! not yet supported  
        
            write(msg_file,*)'more than 2 broken edges not yet supported!'
          
            ! partition only according to the first two broken edges
            nfailedge=2
            goto 10
          
        else
        
            write(msg_file,*)'unknown no. of nfailedge!'
            call exit_function
          
        end if
             
            
!       update element curr_status and sub-element cnc matrices

        if(elstat>elem%curr_status) then
            elem%curr_status=elstat                       
            call update_subcnc(elem,edgstat,ifedg,nfailedge)
        end if







!-----------------------------------------------------------------------!
!                   UPDATE INTERFACE
!               update global libraries
!-----------------------------------------------------------------------!

        ! update glb edge array, only the broken edges' status variables
        lib_edge(elem%edgecnc(ifedg(:)))=edgstat(ifedg(:))
        
        ! update glb node array, only the broken edges' fl. node coord
        do i=1, nfailedge
            j=ifedg(i)          ! local index of broken edge
            
            l=topo(3,j)         ! elem lcl index of broken edge fl. node 1
            k=elem%nodecnc(l)   ! global index of broken edge fl. node 1
            call update(lib_node(k),x=coord(l)%array)
            
            l=topo(4,j)         ! elem lcl index of broken edge fl. node 2
            k=elem%nodecnc(l)   ! global index of broken edge fl. node 2
            call update(lib_node(k),x=coord(l)%array)
        end do






!       deallocate local dynamic arrays

        deallocate(edgstat)
        deallocate(ifedg)
    
    
    end subroutine edge_status_partition
    
  





!********************************************************************************************
!******************* subroutine kplyfail ****************************************************
!*********** quadratic stress failure criteria and element partition criterion **************
!********************************************************************************************
      subroutine kplyfail(nst,nig,sig,yt,s,fstat,pstat,element,
     & ndim,xelm,nnode,cncsub,mxnd,theta,jelm)

!     ! passed in variables
      real(kind=dp),intent(in) :: sig(nst,nig),yt,s,theta
      integer,intent(in) :: jelm
      character(len=10),intent(in) :: element
      real(kind=dp),intent(inout) :: xelm(ndim,nnode),fstat,pstat
      integer,intent(inout) :: nst,nig,ndim,nnode,mxnd,cncsub(mxnd+1,*)     
      ! local variables
      real(kind=dp) :: ff,xo,yo,xct,yct
      real(kind=dp) :: xp1,xp2,yp1,yp2 ! precrack is (xp1,yp1) to (xp2,yp2)
      real(kind=dp) :: x1,x2,y1,y2 ! elm edge is (x1,y1) to (x2,y2)
      integer :: ig, cntr, i, j, k, iscross
      integer :: kfedg, jfnd, jfnd2
      integer,allocatable :: edg(:,:) ! edg(:,j): fl. nodes on edge j 
      integer,allocatable :: edgstat(:) !edgstat: status of edge, 0: intact, 1: broken
      integer,allocatable :: ifedg(:)
!
      ! initialize local variables
      ff=zero     
      xo=zero;yo=zero
      xct=zero;yct=zero
      xp1=zero;xp2=zero;yp1=zero;yp2=zero
      x1=zero;x2=zero;y1=zero;y2=zero
      ig=0;cntr=0;i=0;j=0;k=0;iscross=0
      kfedg=0;jfnd=0;jfnd2=0
	  
        
        allocate(edgstat(nedge),ifedg(nedge))
        edgstat(:)=0 !initialize edgstat
        ifedg(:)=0


    ! failure criterion
        
      if((yt .eq. zero) .or. (s .eq. zero)) then
        write(6,*) 'zero strength value!'
        ff=one
      else
        do ig=1,nig
            if(nst .eq. 3) then ! bulk element
            ff=(max(zero,sig(2,ig))/yt)**2+(sig(3,ig)/s)**2
            else if(nst .eq. 2) then ! cohesive element
            ff=(max(zero,sig(1,ig))/yt)**2+(sig(2,ig)/s)**2
            else
                write(6,*) 'unsupported no. of strains!'
                call xit
            end if	
        end do
      end if


            
      if(ff .ge. one) then
        
          ! find xp1, yp1
          if(fstat .eq. zero) then ! element originally intact
            ! find centroid
            xo=quarter*(xelm(1,1)+xelm(1,2)+xelm(1,3)+xelm(1,4))
            yo=quarter*(xelm(2,1)+xelm(2,2)+xelm(2,3)+xelm(2,4))
            xp1=xo
            yp1=yo
            ! find xp2, yp2
            xp2=xp1+cos(theta/halfcirc*pi)
            yp2=yp1+sin(theta/halfcirc*pi)
            do i=1,nedge 
                ! tip corods of edge i
                x1=xelm(1,edg(1,i))
                y1=xelm(2,edg(1,i))
                x2=xelm(1,edg(2,i))
                y2=xelm(2,edg(2,i))
                iscross=0
                xct=zero
                yct=zero
                call klinecross(x1,y1,x2,y2,xp1,yp1,xp2,yp2,
     &              iscross,xct,yct)
                if (iscross.gt.0) then
                    edgstat(i)=4 ! edge i cracked
                    kfedg=kfedg+1
                    xelm(1,edg(3,i))=xct ! store c tip coords on edge i fl. nd 1
                    xelm(2,edg(3,i))=yct
                    xelm(1,edg(4,i))=xct ! store c tip coords on edge i fl. nd 2
                    xelm(2,edg(4,i))=yct
                    !jfnd=min(ndelm(edg(3,i),jelm),ndelm(edg(4,i),jelm))! find the smaller fnode on this edge
                    jfnd=ndelm(edg(3,i),jelm)
                    fnode(1,jfnd)=four ! update its status
                    fnode(2,jfnd)=xct
                    fnode(3,jfnd)=yct
                    jfnd=ndelm(edg(4,i),jelm)
                    fnode(1,jfnd)=four ! update its status
                    fnode(2,jfnd)=xct
                    fnode(3,jfnd)=yct
                 endif
                 if(kfedg.eq.2) exit ! found 2 broken edges already
            end do
          
          else ! element already partitioned
            !---------- find no. of broken edges -------------------------
            do i=1,nedge
                jfnd=min(ndelm(edg(3,i),jelm),ndelm(edg(4,i),jelm))! find the smaller fnode on this edge
                if(fnode(1,jfnd).gt.zero) then
                    !edgstat(i)=int(fnode(1,jfnd))
                    kfedg=kfedg+1
                    ifedg(kfedg)=i              
                end if
            end do
            !-------------------------------------------------------------
            ! find (xp1,yp1), (xp2,yp2)
            if (fstat .eq. one) then ! transition elm, already has an edge partitioned
                if(kfedg.ne.1) then 
                 write(6,*)'inconsistency in kplyfail,fstat=1!'
                 call xit
                else !kfedg=1
                 j=ifedg(1)
                 jfnd=min(ndelm(edg(3,j),jelm),ndelm(edg(4,j),jelm))
                 ! update edgstat(j) & fnode(jfnd)
                 edgstat(j)=4 ! cohesive crack
                 fnode(1,jfnd)=four
                 ! find xp1, yp1
                 xp1=fnode(2,jfnd)           
                 yp1=fnode(3,jfnd)
                 ! update fnode status of the other fnode on the edge
                 jfnd2=max(ndelm(edg(3,j),jelm),ndelm(edg(4,j),jelm))
                 do k=1,3
                    fnode(k,jfnd2)=fnode(k,jfnd)
                 end do                 
                 ! find xp2, yp2
                 xp2=xp1+cos(theta/halfcirc*pi)
                 yp2=yp1+sin(theta/halfcirc*pi)
                 ! next, find the edge crossed by the crack line
                 do i=1,nedge
                    if (i.eq.j) cycle ! the already cracked edge,go to next edge
                    ! tip corods of edge i
                    x1=xelm(1,edg(1,i))
                    y1=xelm(2,edg(1,i))
                    x2=xelm(1,edg(2,i))
                    y2=xelm(2,edg(2,i))
                    iscross=0
                    xct=zero
                    yct=zero
                    call klinecross(x1,y1,x2,y2,xp1,yp1,xp2,yp2,
     &              iscross,xct,yct)
                    if (iscross.gt.0) then
                        edgstat(i)=4 ! edge i cracked
                        kfedg=kfedg+1
                        xelm(1,edg(3,i))=xct ! store c tip coords on edge i fl. nd 1
                        xelm(2,edg(3,i))=yct
                        xelm(1,edg(4,i))=xct ! store c tip coords on edge i fl. nd 2
                        xelm(2,edg(4,i))=yct
                        !jfnd=min(ndelm(edg(3,i),jelm),ndelm(edg(4,i),jelm))! find the smaller fnode on this edge
                        jfnd=ndelm(edg(3,i),jelm)
                        fnode(1,jfnd)=four ! update its status
                        fnode(2,jfnd)=xct
                        fnode(3,jfnd)=yct
                        jfnd=ndelm(edg(4,i),jelm)
                        fnode(1,jfnd)=four ! update its status
                        fnode(2,jfnd)=xct
                        fnode(3,jfnd)=yct
                     endif
                     if(kfedg.eq.2) exit ! found 2 broken edges already
                 end do
                endif
            elseif (fstat.gt.one .and. fstat.lt.five) then ! element already has two edges partitioned
                if(kfedg.lt.2) then 
                 write(6,*)'inconsistency in kplyfail,fstat>1!'
                 call xit
                else
                 kfedg=2 ! ignore more than 2 broken edges at the moment
                 ! update first edge status var & fl. node status var
                 j=ifedg(1)
                 jfnd=min(ndelm(edg(3,j),jelm),ndelm(edg(4,j),jelm))
                 edgstat(j)=4 ! cohesive crack
                 fnode(1,jfnd)=four
                 ! update fnode status of the other fnode on the edge
                 jfnd2=max(ndelm(edg(3,j),jelm),ndelm(edg(4,j),jelm))
                 do k=1,3
                    fnode(k,jfnd2)=fnode(k,jfnd)
                 end do
                 ! update second edge status var & fl. node status var
                 j=ifedg(2)
                 jfnd=min(ndelm(edg(3,j),jelm),ndelm(edg(4,j),jelm))
                 edgstat(j)=4 ! cohesive crack
                 fnode(1,jfnd)=four
                 ! update fnode status of the other fnode on the edge
                 jfnd2=max(ndelm(edg(3,j),jelm),ndelm(edg(4,j),jelm))
                 do k=1,3
                    fnode(k,jfnd2)=fnode(k,jfnd)
                 end do
                endif         
            else
                write(6,*) 'unsupported fstat value for failure!'
                call xit          
            endif
          
          end if
          
          ! update the fstat, pstat, cncsub
          fstat=five
          call subcnc(jelm,element,edg,nedge,edgstat,kfedg,
     &    pstat,cncsub,mxnd)
          
          
            
      end if
!
      deallocate(edgstat)
      deallocate(ifedg)
      deallocate(edg)
        
      return  
      end subroutine kplyfail






       
    subroutine update_subcnc(elem,edgstat,ifedg,nfailedge)
    
    ! passed-in variables
    type(xquad_element),    intent(inout)   :: elem
    integer,                intent(in)      :: edgstat(:), ifedg(:), nfailedge



    ! local variables
    type(int_alloc_array), allocatable :: subglbcnc(:)  ! glb cnc of sub elements
    integer :: i, j, l                                  ! counters
    integer :: ibe, ibe1, ibe2                          ! indices of broken edges
    integer :: e1,e2,e3,e4                              ! edge indices, used for partitioning element
    integer :: nsub, nbulk                              ! no. of sub elements, and no. of bulk partitions
    integer :: jnode, jnode1, jnode2, jnode3, jnode4    ! node index variables



!       initialize local variables

        i=0; j=0; l=0
        e1=0; e2=0; e3=0; e4=0
        ibe=0; ibe1=0; ibe2=0
        nsub=0; nbulk=0
        jnode=0; jnode1=0; jnode2=0; jnode3=0; jnode4=0
        


10      select case (nfailedge)
            case (0) !- no cracked edge, do nothing
                continue
            
            
            case (1) !- one edge cracked, trans partition
                ! find the index of the broken edge
                ibe=ifedg(1)

                ! verify its status variable value
                if(edgstat(ibe)/=1) then
                    write(msg_file,*)'transition partition only accepts edgstat=1!'
                    call exit_function
                end if
                
                ! allocate sub element arrays; in this case, 3 tri elements
                nsub=3
                allocate(elem%subelem(nsub))
                allocate(elem%subcnc(nsub))
                allocate(subglbcnc(nsub))
                do j=1, nsub
                    allocate(elem%subcnc(j)%array(3))
                    allocate(subglbcnc(j)%array(3))
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
                
                subglbcnc(1)%array(:)=elem%nodecnc(elem%subcnc(1)%array(:))
                
                ! sub elm 2 connec
                elem%subcnc(2)%array(1)=topo(1,e2)
                elem%subcnc(2)%array(2)=topo(2,e2)
                elem%subcnc(2)%array(3)=jnode  

                subglbcnc(2)%array(:)=elem%nodecnc(elem%subcnc(2)%array(:))
                
                ! sub elm 3 connec
                elem%subcnc(3)%array(1)=topo(1,e3)
                elem%subcnc(3)%array(2)=topo(2,e3)
                elem%subcnc(3)%array(3)=jnode  

                subglbcnc(3)%array(:)=elem%nodecnc(elem%subcnc(3)%array(:))

                ! create sub elements
                call prepare(elem%subelem(1),eltype='tri', matkey=elem%bulkmat, glbcnc=subglbcnc(1)%array, theta=elem%theta)
                call prepare(elem%subelem(2),eltype='tri', matkey=elem%bulkmat, glbcnc=subglbcnc(2)%array, theta=elem%theta)
                call prepare(elem%subelem(3),eltype='tri', matkey=elem%bulkmat, glbcnc=subglbcnc(3)%array, theta=elem%theta)
         

 
         case (2) !- two edges cracked
         
            ibe1=min(ifedg(1),ifedg(2))   ! local edge index of 1st broken edge
            ibe2=max(ifedg(1),ifedg(2))   ! local edge index of 2nd broken edge
            if(edgstat(ibe1)==4 .or. edgstat(ibe2)==4) iscoh=.true.
            
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
                    end select
                case(2)
                    select case(ibe2)
                        case(3)
                            nbulk=4
                            e1=2; e2=3; e3=4; e4=1
                        case(4)
                            nbulk=2
                            e1=2; e2=3; e3=4; e4=1
                    end select
                case(3)
                    if(ibe2==4) then
                        nbulk=4
                        e1=3; e2=4; e3=1; e4=2
                    end if    
                case default
                        write(msg_file,*)'wrong broken edge in update subcnc xquad'
                        call exit_function
            end select
            
            
            select case(nbulk)
                case(2)
                ! two quad bulk subdomains
                    if(iscoh) then
                        nsub=3  ! two quad and one coh2d
                    else
                        nsub=2  ! only two quad
                    end if
                    allocate(elem%subelem(nsub))
                    allocate(elem%subcnc(nsub))
                    allocate(subglbcnc(nsub))
                    do j=1, nsub
                        allocate(elem%subcnc(j)%array(4))
                        allocate(subglbcnc(j)%array(4))
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
                    elem%subcnc(1)%array(2)=topo(3,e1); if(edgstat(e1)<4) elem%subcnc(1)%array(2)=jnode1
                    elem%subcnc(1)%array(3)=topo(4,e3); if(edgstat(e3)<4) elem%subcnc(1)%array(3)=jnode3
                    elem%subcnc(1)%array(4)=topo(2,e3)
                    
                    subglbcnc(1)%array(:)=elem%nodecnc(elem%subcnc(1)%array(:))
                    
                    ! sub elm 2 connec
                    elem%subcnc(2)%array(1)=topo(4,e1); if(edgstat(e1)<4) elem%subcnc(2)%array(1)=jnode1
                    elem%subcnc(2)%array(2)=topo(2,e1)
                    elem%subcnc(2)%array(3)=topo(1,e3)
                    elem%subcnc(2)%array(4)=topo(3,e3); if(edgstat(e3)<4) elem%subcnc(2)%array(4)=jnode3

                    subglbcnc(2)%array(:)=elem%nodecnc(elem%subcnc(2)%array(:))
                    
                    ! create sub bulk elements
                    call prepare(elem%subelem(1),eltype='quad', matkey=elem%bulkmat, glbcnc=subglbcnc(1)%array, theta=elem%theta)
                    call prepare(elem%subelem(2),eltype='quad', matkey=elem%bulkmat, glbcnc=subglbcnc(2)%array, theta=elem%theta)
                    
                    if(iscoh) then
                    
                    ! sub elm 3 connec
                    elem%subcnc(3)%array(1)=topo(4,e3) if(edgstat(e3)<4) elem%subcnc(3)%array(1)=jnode3
                    elem%subcnc(3)%array(2)=topo(3,e1) if(edgstat(e1)<4) elem%subcnc(3)%array(2)=jnode1
                    elem%subcnc(3)%array(3)=topo(4,e1) if(edgstat(e1)<4) elem%subcnc(3)%array(3)=jnode1
                    elem%subcnc(3)%array(4)=topo(3,e3) if(edgstat(e3)<4) elem%subcnc(3)%array(4)=jnode3
                    
                    subglbcnc(3)%array(:)=elem%nodecnc(elem%subcnc(3)%array(:))
                    
                    call prepare(elem%subelem(3),eltype='coh2d', matkey=elem%cohmat, glbcnc=subglbcnc(3)%array)
                    
                    end if
                    
                case(4)
                ! four triangular bulk subdomains
                    if(iscoh) then
                        nsub=5
                    else
                        nsub=4
                    end if
                    allocate(elem%subelem(nsub))
                    allocate(elem%subcnc(nsub))
                    allocate(subglbcnc(nsub))
                    do j=1, 4   ! bulk elem cnc
                        allocate(elem%subcnc(j)%array(3))
                        allocate(subglbcnc(j)%array(3))
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
                    elem%subcnc(1)%array(1)=topo(4,e1); if(edgstat(e1)<4) elem%subcnc(1)%array(1)=jnode1
                    elem%subcnc(1)%array(2)=topo(1,e2)
                    elem%subcnc(1)%array(3)=topo(3,e2); if(edgstat(e2)<4) elem%subcnc(1)%array(3)=jnode2                    
                    
                    subglbcnc(1)%array(:)=elem%nodecnc(elem%subcnc(1)%array(:))
                    
                    ! sub elm 2 connec
                    elem%subcnc(2)%array(1)=topo(4,e2); if(edgstat(e2)<4) elem%subcnc(2)%array(1)=jnode2
                    elem%subcnc(2)%array(2)=topo(1,e3)
                    elem%subcnc(2)%array(3)=topo(2,e3)
                    
                    subglbcnc(2)%array(:)=elem%nodecnc(elem%subcnc(2)%array(:))
                    
                    ! sub elm 3 connec
                    elem%subcnc(3)%array(1)=topo(1,e4)
                    elem%subcnc(3)%array(2)=topo(2,e4)
                    elem%subcnc(3)%array(3)=topo(3,e1); if(edgstat(e1)<4) elem%subcnc(3)%array(3)=jnode1
                    
                    subglbcnc(3)%array(:)=elem%nodecnc(elem%subcnc(3)%array(:))
                    
                    ! sub elm 4 connec
                    elem%subcnc(4)%array(1)=topo(3,e1); if(edgstat(e1)<4) elem%subcnc(4)%array(1)=jnode1
                    elem%subcnc(4)%array(2)=topo(4,e2); if(edgstat(e2)<4) elem%subcnc(4)%array(2)=jnode2
                    elem%subcnc(4)%array(3)=topo(2,e3) 
                    
                    subglbcnc(4)%array(:)=elem%nodecnc(elem%subcnc(4)%array(:))
                    
                    ! create sub bulk elements
                    call prepare(elem%subelem(1),eltype='tri', matkey=elem%bulkmat, glbcnc=subglbcnc(1)%array, theta=elem%theta)
                    call prepare(elem%subelem(2),eltype='tri', matkey=elem%bulkmat, glbcnc=subglbcnc(2)%array, theta=elem%theta)
                    call prepare(elem%subelem(3),eltype='tri', matkey=elem%bulkmat, glbcnc=subglbcnc(3)%array, theta=elem%theta)
                    call prepare(elem%subelem(4),eltype='tri', matkey=elem%bulkmat, glbcnc=subglbcnc(4)%array, theta=elem%theta)
                    
                    if(iscoh) then
                    
                    ! sub elm 5 connec
                    elem%subcnc(5)%array(1)=topo(4,e1) if(edgstat(e1)<4) elem%subcnc(5)%array(1)=jnode1
                    elem%subcnc(5)%array(2)=topo(3,e2) if(edgstat(e2)<4) elem%subcnc(5)%array(2)=jnode2
                    elem%subcnc(5)%array(3)=topo(4,e2) if(edgstat(e2)<4) elem%subcnc(5)%array(3)=jnode2
                    elem%subcnc(5)%array(4)=topo(3,e1) if(edgstat(e1)<4) elem%subcnc(5)%array(4)=jnode1
                    
                    subglbcnc(5)%array(:)=elem%nodecnc(elem%subcnc(5)%array(:))
                    
                    call prepare(elem%subelem(5),eltype='coh2d', matkey=elem%cohmat, glbcnc=subglbcnc(5)%array)
                    
                    end if
                    
                case default
                    write(msg_file,*)'wrong nbulk in update subcnc xquad'
                    call exit_function
            end select

          
         case(3) !- three edges crack
           write(msg_file,*) 'three-edge crack partition not yet supported!'
           nfailedge=2
           goto 10
           
         case(4) !- four edges crack
           write(msg_file,*) 'four-edge crack partition not yet supported!'
           nfailedge=2
           goto 10
           
         case default
           write(msg_file,*) 'WARNING: xquad update subcnc case selection default!'
           
        end select
        
        
        ! deallocate local array
        
        deallocate(subglbcnc)


    end subroutine update_subcnc


  
  
  
  
  
  
  
  
  
  
end module xquad_element_module