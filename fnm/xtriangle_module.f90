    module xtriangle_module
      use parameter_module
      
      implicit none
      private


      type, public :: xtriangle ! breakable triangle
        integer :: end_nodes(3)
        integer :: edges(3)
        integer,allocatable :: floating_nodes(:)
        integer :: current_status
        integer :: parent
        integer,allocatable :: children(:)
      end type xtriangle
      
      interface empty
        module procedure empty_xtriangle
      end interface
      
      interface update
        module procedure update_xtriangle
      end interface

      public :: empty,update


 
      contains
      
      
      ! empty a breakable triangle
      subroutine empty_xtriangle(this_xtriangle)
      
      	type(xtriangle),intent(inout) :: this_xtriangle
        
        integer :: istat
      	
        this_xtriangle%end_nodes(:)=0
        this_xtriangle%edges(:)=0
        this_xtriangle%current_status=0
        this_xtriangle%parent=0

        if(allocated(this_xtriangle%floating_nodes)) then
        deallocate(this_xtriangle%floating_nodes,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xtriangle**"
        end if

        if(allocated(this_xtriangle%children)) then
        deallocate(this_xtriangle%children,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xtriangle**"
        end if

      end subroutine empty_xtriangle
 


     
      ! update a breakable triangle
      subroutine update_xtriangle(this_xtriangle,current_status,end_nodes,&
      & edges,floating_nodes,parent,children)
      
      	type(xtriangle),intent(inout) :: this_xtriangle
        integer,optional,intent(in) :: current_status,parent
        integer,optional,intent(in) :: end_nodes(:),edges(:),floating_nodes(:),children(:)
        
        integer :: istat
      	
        if(present(current_status)) this_xtriangle%current_status=current_status
        
        if(present(parent)) this_xtriangle%parent=parent
        
        if(present(end_nodes)) then
            if(size(end_nodes)==size(this_xtriangle%end_nodes)) then
                this_xtriangle%end_nodes(:)=end_nodes(:)
            else
                stop"**wrong size for xtriangle end_nodes component**"
            end if
        end if
        
        if(present(edges)) then
            if(size(edges)==size(this_xtriangle%edges)) then
                this_xtriangle%edges(:)=edges(:)
            else
                stop"**wrong size for xtriangle edges component**"
            end if
        end if
        
        if(present(floating_nodes)) then
            if(allocated(this_xtriangle%floating_nodes)) then
                if(size(floating_nodes)==size(this_xtriangle%floating_nodes)) then
                    this_xtriangle%floating_nodes(:)=floating_nodes(:)
                else
                    deallocate(this_xtriangle%floating_nodes,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xtriangle**"
                    allocate(this_xtriangle%floating_nodes(size(floating_nodes)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xtriangle**"
                    this_xtriangle%floating_nodes(:)=floating_nodes(:)
                end if         
            else
                allocate(this_xtriangle%floating_nodes(size(floating_nodes)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xtriangle**"
                this_xtriangle%floating_nodes(:)=floating_nodes(:)            
            end if
        end if
        
        if(present(children)) then
            if(allocated(this_xtriangle%children)) then
                if(size(children)==size(this_xtriangle%children)) then
                    this_xtriangle%children(:)=children(:)
                else
                    deallocate(this_xtriangle%children,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xtriangle**"
                    allocate(this_xtriangle%children(size(children)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xtriangle**"
                    this_xtriangle%children(:)=children(:)
                end if         
            else
                allocate(this_xtriangle%children(size(children)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xtriangle**"
                this_xtriangle%children(:)=children(:)            
            end if
        end if

      end subroutine update_xtriangle 
      
      
      
    end module xtriangle_module