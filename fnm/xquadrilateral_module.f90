    module xquadrilateral_module
      use parameter_module
      
      implicit none
      private


      type, public :: xquadrilateral ! breakable quadrilateral
        integer :: end_nodes(4)
        integer :: edges(4)
        integer,allocatable :: floating_nodes(:)
        integer :: current_status
        integer :: parent
        integer,allocatable :: children(:)
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
      	
        this_xquadrilateral%end_nodes(:)=0
        this_xquadrilateral%edges(:)=0
        this_xquadrilateral%current_status=0
        this_xquadrilateral%parent=0

        if(allocated(this_xquadrilateral%floating_nodes)) then
        deallocate(this_xquadrilateral%floating_nodes,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xquadrilateral**"
        end if

        if(allocated(this_xquadrilateral%children)) then
        deallocate(this_xquadrilateral%children,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xquadrilateral**"
        end if

      end subroutine empty_xquadrilateral
 


     
      ! update a breakable quadrilateral
      subroutine update_xquadrilateral(this_xquadrilateral,current_status,end_nodes,&
      & edges,floating_nodes,parent,children)
      
      	type(xquadrilateral),intent(inout) :: this_xquadrilateral
        integer,optional,intent(in) :: current_status,parent
        integer,optional,intent(in) :: end_nodes(:),edges(:),floating_nodes(:),children(:)
        
        integer :: istat
      	
        if(present(current_status)) this_xquadrilateral%current_status=current_status
        
        if(present(parent)) this_xquadrilateral%parent=parent
        
        if(present(end_nodes)) then
            if(size(end_nodes)==size(this_xquadrilateral%end_nodes)) then
                this_xquadrilateral%end_nodes(:)=end_nodes(:)
            else
                stop"**wrong size for xquadrilateral end_nodes component**"
            end if
        end if
        
        if(present(edges)) then
            if(size(edges)==size(this_xquadrilateral%edges)) then
                this_xquadrilateral%edges(:)=edges(:)
            else
                stop"**wrong size for xquadrilateral edges component**"
            end if
        end if
        
        if(present(floating_nodes)) then
            if(allocated(this_xquadrilateral%floating_nodes)) then
                if(size(floating_nodes)==size(this_xquadrilateral%floating_nodes)) then
                    this_xquadrilateral%floating_nodes(:)=floating_nodes(:)
                else
                    deallocate(this_xquadrilateral%floating_nodes,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xquadrilateral**"
                    allocate(this_xquadrilateral%floating_nodes(size(floating_nodes)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xquadrilateral**"
                    this_xquadrilateral%floating_nodes(:)=floating_nodes(:)
                end if         
            else
                allocate(this_xquadrilateral%floating_nodes(size(floating_nodes)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xquadrilateral**"
                this_xquadrilateral%floating_nodes(:)=floating_nodes(:)            
            end if
        end if
        
        if(present(children)) then
            if(allocated(this_xquadrilateral%children)) then
                if(size(children)==size(this_xquadrilateral%children)) then
                    this_xquadrilateral%children(:)=children(:)
                else
                    deallocate(this_xquadrilateral%children,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xquadrilateral**"
                    allocate(this_xquadrilateral%children(size(children)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xquadrilateral**"
                    this_xquadrilateral%children(:)=children(:)
                end if         
            else
                allocate(this_xquadrilateral%children(size(children)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xquadrilateral**"
                this_xquadrilateral%children(:)=children(:)            
            end if
        end if

      end subroutine update_xquadrilateral      
      
      
    end module xquadrilateral_module