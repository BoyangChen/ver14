    module xnode_module
    use parameter_module
    
      implicit none
      private
    
      type, public :: xnode ! a node with enriched d.o.f
        real(kind=dp) :: x(3) ! coordinates, density
        real(kind=dp) :: u(3),du(3),v(3) ! displacements, incremental disp. and velocity
        real(kind=dp),allocatable :: dof(:),ddof(:) ! additional dof and incremental dof
      end type xnode
      
      interface empty
        module procedure empty_xnode
      end interface
      
      interface update
        module procedure update_xnode
      end interface

      public :: empty,update



      
      contains



      
      ! empty a node
      subroutine empty_xnode(this_xnode)
      
      	type(xnode),intent(inout) :: this_xnode
        
        integer :: istat
      	
        this_xnode%x(:)=zero
        this_xnode%u(:)=zero
        this_xnode%du(:)=zero
        this_xnode%v(:)=zero

        if(allocated(this_xnode%dof)) then
        deallocate(this_xnode%dof,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xnode**"
        end if

        if(allocated(this_xnode%ddof)) then
        deallocate(this_xnode%ddof,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xnode**"
        end if

      end subroutine empty_xnode
    



      ! update a node
      subroutine update_xnode(this_xnode,x,u,du,v,dof,ddof)
      
      	type(xnode),intent(inout) :: this_xnode
        real(kind=dp),optional,intent(in) :: x(:),u(:),du(:),v(:)
        real(kind=dp),optional,intent(in) :: dof(:),ddof(:)
        
        integer :: istat
        
        if(present(x)) then
            if(size(x)==size(this_xnode%x)) then
                this_xnode%x(:)=x(:)
            else
                stop"**wrong size for xnode x component**"
            end if
        end if
        
        if(present(u)) then
            if(size(u)==size(this_xnode%u)) then
                this_xnode%u(:)=u(:)
            else
                stop"**wrong size for xnode u component**"
            end if
        end if
        
        if(present(du)) then
            if(size(du)==size(this_xnode%du)) then
                this_xnode%du(:)=du(:)
            else
                stop"**wrong size for xnode du component**"
            end if
        end if
        
        if(present(v)) then
            if(size(v)==size(this_xnode%v)) then
                this_xnode%v(:)=v(:)
            else
                stop"**wrong size for xnode v component**"
            end if
        end if
        
        if(present(dof)) then
            if(allocated(this_xnode%dof)) then
                if(size(dof)==size(this_xnode%dof)) then
                    this_xnode%dof(:)=dof(:)
                else
                    deallocate(this_xnode%dof,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xnode**"
                    allocate(this_xnode%dof(size(dof)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xnode**"
                    this_xnode%dof(:)=dof(:)
                end if         
            else
                allocate(this_xnode%dof(size(dof)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xnode**"
                this_xnode%dof(:)=dof(:)            
            end if
        end if
        
        if(present(ddof)) then
            ! check if ddof dimension matches dof dimension
            if(.not.allocated(this_xnode%dof)) then
                stop"**dof must be allocated before ddof in update_xnode**"
            else if(size(ddof)/=size(this_xnode%dof)) then
                stop"**ddof size must be consistent with dof size in update_xnode**"
            end if           
            ! update ddof values
            if(allocated(this_xnode%ddof)) then
                if(size(ddof)==size(this_xnode%ddof)) then
                    this_xnode%ddof(:)=ddof(:)
                else
                    deallocate(this_xnode%ddof,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xnode**"
                    allocate(this_xnode%ddof(size(ddof)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xnode**"
                    this_xnode%ddof(:)=ddof(:)
                end if         
            else
                allocate(this_xnode%ddof(size(ddof)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xnode**"
                this_xnode%ddof(:)=ddof(:)            
            end if
        end if

      end subroutine update_xnode 
      
      

    
    end module xnode_module