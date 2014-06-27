    module xquadrilateral_module
      use parameter_module
      use subelement_module
      
      implicit none
      private

      integer,parameter :: nnode=4, nedge=4
      

      type, public :: xquadrilateral ! breakable quadrilateral
        integer :: end_node(nnode) ! cnc to glb node arrays for accessing glb real(vertex) node numbers
        integer :: edge(nedge) ! cnc to glb edge arrays for accessing status var. and glb flo node numbers
        integer,allocatable :: flo_node(:) ! assigned to this surface, in addition to the flo nodes on edge
        integer :: curr_status
        type(subelement),allocatable :: sub_elem(:) ! sub element connectivities
      end type xquadrilateral
      
      interface empty
        module procedure empty_xquadrilateral
      end interface
      
      interface update
        module procedure update_xquadrilateral
      end interface

      public :: empty,update


 
      contains




      ! empty a breakable quadrilateral
      subroutine empty_xquadrilateral(this_xquadrilateral)
      
      	type(xquadrilateral),intent(inout) :: this_xquadrilateral
        
        integer :: istat
      	
        this_xquadrilateral%end_node(:)=0
        this_xquadrilateral%edge(:)=0
        this_xquadrilateral%curr_status=0

        if(allocated(this_xquadrilateral%flo_node)) then
        deallocate(this_xquadrilateral%flo_node,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xquadrilateral**"
        end if

      end subroutine empty_xquadrilateral
 


     
      ! update a breakable quadrilateral
      subroutine update_xquadrilateral(this_xquadrilateral,curr_status,end_node,&
      & edge,flo_node)
      
      	type(xquadrilateral),intent(inout) :: this_xquadrilateral
        integer,optional,intent(in) :: curr_status
        integer,optional,intent(in) :: end_node(:),edge(:),flo_node(:)
        
        integer :: istat
      	
        if(present(curr_status)) this_xquadrilateral%curr_status=curr_status
        
        if(present(end_node)) then
            if(size(end_node)==size(this_xquadrilateral%end_node)) then
                this_xquadrilateral%end_node(:)=end_node(:)
            else
                stop"**wrong size for xquadrilateral end_node component**"
            end if
        end if
        
        if(present(edge)) then
            if(size(edge)==size(this_xquadrilateral%edge)) then
                this_xquadrilateral%edge(:)=edge(:)
            else
                stop"**wrong size for xquadrilateral edge component**"
            end if
        end if
        
        if(present(flo_node)) then
            if(allocated(this_xquadrilateral%flo_node)) then
                if(size(flo_node)==size(this_xquadrilateral%flo_node)) then
                    this_xquadrilateral%flo_node(:)=flo_node(:)
                else
                    deallocate(this_xquadrilateral%flo_node,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xquadrilateral**"
                    allocate(this_xquadrilateral%flo_node(size(flo_node)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xquadrilateral**"
                    this_xquadrilateral%flo_node(:)=flo_node(:)
                end if         
            else
                allocate(this_xquadrilateral%flo_node(size(flo_node)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xquadrilateral**"
                this_xquadrilateral%flo_node(:)=flo_node(:)            
            end if
        end if

      end subroutine update_xquadrilateral      
      
      
    end module xquadrilateral_module