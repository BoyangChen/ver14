    module xedge_module
      use parameter_module
      
      implicit none
      private
      
      type, public :: edge ! breakable edge
        integer :: end_node(2)
      end type edge 
      
      type, public :: xedge ! breakable edge
        integer :: end_node(2)
        integer,allocatable :: flo_node(:)
        integer :: curr_status
      end type xedge 

      interface empty
        module procedure empty_xedge
      end interface
      
      interface update
        module procedure update_xedge
      end interface

      public :: empty,update


 
      contains


      
      ! empty a breakable edge
      subroutine empty_xedge(this_xedge)
      
      	type(xedge),intent(inout) :: this_xedge
        
        integer :: istat
      	
        this_xedge%end_node(:)=0
        this_xedge%curr_status=0

        if(allocated(this_xedge%flo_node)) then
        deallocate(this_xedge%flo_node,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xedge**"
        end if

      end subroutine empty_xedge
 


     
      ! update a breakable edge
      subroutine update_xedge(this_xedge,curr_status,end_node,&
      & flo_node)
      
      	type(xedge),intent(inout) :: this_xedge
        integer,optional,intent(in) :: curr_status
        integer,optional,intent(in) :: end_node(:),flo_node(:)
        
        integer :: istat
      	
        if(present(curr_status)) this_xedge%curr_status=curr_status
        
        if(present(end_node)) then
            if(size(end_node)==size(this_xedge%end_node)) then
                this_xedge%end_node(:)=end_node(:)
            else
                stop"**wrong size for xedge end_node component**"
            end if
        end if
        
        if(present(flo_node)) then
            if(allocated(this_xedge%flo_node)) then
                if(size(flo_node)==size(this_xedge%flo_node)) then
                    this_xedge%flo_node(:)=flo_node(:)
                else
                    deallocate(this_xedge%flo_node,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xedge**"
                    allocate(this_xedge%flo_node(size(flo_node)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xedge**"
                    this_xedge%flo_node(:)=flo_node(:)
                end if         
            else
                allocate(this_xedge%flo_node(size(flo_node)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xedge**"
                this_xedge%flo_node(:)=flo_node(:)            
            end if
        end if
        

      end subroutine update_xedge   
    
    
    end module xedge_module