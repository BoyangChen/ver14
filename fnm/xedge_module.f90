    module xedge_module
      use parameter_module
      
      implicit none
      private
      
      type, public :: xedge ! breakable edge
        integer :: end_nodes(2)
        integer,allocatable :: floating_nodes(:)
        integer :: current_status
        integer :: parent
        integer,allocatable :: children(:)
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
      	
        this_xedge%end_nodes(:)=0
        this_xedge%current_status=0
        this_xedge%parent=0

        if(allocated(this_xedge%floating_nodes)) then
        deallocate(this_xedge%floating_nodes,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xedge**"
        end if

        if(allocated(this_xedge%children)) then
        deallocate(this_xedge%children,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xedge**"
        end if

      end subroutine empty_xedge
 


     
      ! update a breakable edge
      subroutine update_xedge(this_xedge,current_status,end_nodes,&
      & floating_nodes,parent,children)
      
      	type(xedge),intent(inout) :: this_xedge
        integer,optional,intent(in) :: current_status,parent
        integer,optional,intent(in) :: end_nodes(:),floating_nodes(:),children(:)
        
        integer :: istat
      	
        if(present(current_status)) this_xedge%current_status=current_status
        
        if(present(parent)) this_xedge%parent=parent
        
        if(present(end_nodes)) then
            if(size(end_nodes)==size(this_xedge%end_nodes)) then
                this_xedge%end_nodes(:)=end_nodes(:)
            else
                stop"**wrong size for xedge end_nodes component**"
            end if
        end if
        
        if(present(floating_nodes)) then
            if(allocated(this_xedge%floating_nodes)) then
                if(size(floating_nodes)==size(this_xedge%floating_nodes)) then
                    this_xedge%floating_nodes(:)=floating_nodes(:)
                else
                    deallocate(this_xedge%floating_nodes,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xedge**"
                    allocate(this_xedge%floating_nodes(size(floating_nodes)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xedge**"
                    this_xedge%floating_nodes(:)=floating_nodes(:)
                end if         
            else
                allocate(this_xedge%floating_nodes(size(floating_nodes)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xedge**"
                this_xedge%floating_nodes(:)=floating_nodes(:)            
            end if
        end if
        
        if(present(children)) then
            if(allocated(this_xedge%children)) then
                if(size(children)==size(this_xedge%children)) then
                    this_xedge%children(:)=children(:)
                else
                    deallocate(this_xedge%children,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xedge**"
                    allocate(this_xedge%children(size(children)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xedge**"
                    this_xedge%children(:)=children(:)
                end if         
            else
                allocate(this_xedge%children(size(children)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xedge**"
                this_xedge%children(:)=children(:)            
            end if
        end if

      end subroutine update_xedge   
    
    
    end module xedge_module