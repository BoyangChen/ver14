    module xquad_element_module
      use parameter_module
      use sub2d_element_module
      
      
      implicit none
      private

      integer,parameter :: nnode=4, nedge=4
      

      type, public :: xquad_element                 ! breakable quadrilateral
        integer :: end_node(nnode)                  ! cnc to glb node arrays for accessing glb real(vertex) node numbers
        integer :: edge(nedge)                      ! cnc to glb edge arrays for accessing status var. and glb flo node numbers
        integer,allocatable :: flo_node(:)          ! assigned to this surface, in addition to the flo nodes on edge
        integer :: curr_status
        type(sub2d_element), allocatable :: subelem(:)
      end type xquad_element
      
      interface empty
        module procedure empty_xquad_element
      end interface
      
      interface update
        module procedure update_xquad_element
      end interface

      public :: empty,update


 
      contains




      ! empty a breakable quadrilateral
      subroutine empty_xquad_element(this_xquad_element)
      
      	type(xquad_element),intent(inout) :: this_xquad_element
        
        integer :: istat
      	
        this_xquad_element%end_node(:)=0
        this_xquad_element%edge(:)=0
        this_xquad_element%curr_status=0

        if(allocated(this_xquad_element%flo_node)) then
        deallocate(this_xquad_element%flo_node,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xquad_element**"
        end if

      end subroutine empty_xquad_element
 


     
      ! update a breakable quadrilateral
      subroutine update_xquad_element(this_xquad_element,curr_status,end_node,&
      & edge,flo_node)
      
      	type(xquad_element),intent(inout) :: this_xquad_element
        integer,optional,intent(in) :: curr_status
        integer,optional,intent(in) :: end_node(:),edge(:),flo_node(:)
        
        integer :: istat
      	
        if(present(curr_status)) this_xquad_element%curr_status=curr_status
        
        if(present(end_node)) then
            if(size(end_node)==size(this_xquad_element%end_node)) then
                this_xquad_element%end_node(:)=end_node(:)
            else
                stop"**wrong size for xquad_element end_node component**"
            end if
        end if
        
        if(present(edge)) then
            if(size(edge)==size(this_xquad_element%edge)) then
                this_xquad_element%edge(:)=edge(:)
            else
                stop"**wrong size for xquad_element edge component**"
            end if
        end if
        
        if(present(flo_node)) then
            if(allocated(this_xquad_element%flo_node)) then
                if(size(flo_node)==size(this_xquad_element%flo_node)) then
                    this_xquad_element%flo_node(:)=flo_node(:)
                else
                    deallocate(this_xquad_element%flo_node,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xquad_element**"
                    allocate(this_xquad_element%flo_node(size(flo_node)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xquad_element**"
                    this_xquad_element%flo_node(:)=flo_node(:)
                end if         
            else
                allocate(this_xquad_element%flo_node(size(flo_node)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xquad_element**"
                this_xquad_element%flo_node(:)=flo_node(:)            
            end if
        end if

      end subroutine update_xquad_element      
      
      
    end module xquad_element_module