    module xnode_module
    use parameter_module
    
      implicit none
      
      private
    
      type, public :: xnode ! a node with enriched d.o.f
      
        private ! hide components from external operation
        
        real(kind=dp) :: x(3)=zero ! coordinates, density
        real(kind=dp) :: u(3)=zero,du(3)=zero,v(3)=zero ! displacements, incremental disp. and velocity
        real(kind=dp),allocatable :: dof(:),ddof(:) ! additional dof and incremental dof
        
      end type xnode
      
      interface empty
        module procedure empty_xnode
      end interface
      
      interface update
        module procedure update_xnode
      end interface
      
      interface export
        module procedure export_xnode
      end interface
      

      public :: empty,update,export



      
      contains





      
      ! empty a node
      subroutine empty_xnode(this_xnode)
      
      	type(xnode),intent(out) :: this_xnode
      	
        this_xnode%x=zero
        this_xnode%u=zero
        this_xnode%du=zero
        this_xnode%v=zero

        if(allocated(this_xnode%dof)) deallocate(this_xnode%dof)

        if(allocated(this_xnode%ddof)) deallocate(this_xnode%ddof)

      end subroutine empty_xnode
      
      
      
    



      ! update a node
      subroutine update_xnode(this_xnode,x,u,du,v,dof,ddof)
      
      	type(xnode),intent(inout) :: this_xnode
        real(kind=dp),optional,intent(in) :: x(:),u(:),du(:),v(:)
        real(kind=dp),optional,intent(in) :: dof(:),ddof(:)
        
        
        if(present(x)) then
            if(size(x)==size(this_xnode%x)) then
                this_xnode%x=x
            else
                write(msg_file,*)"**wrong size for xnode x component**"
                call exit_function
            end if
        end if
        
        if(present(u)) then
            if(size(u)==size(this_xnode%u)) then
                this_xnode%u=u
            else
                write(msg_file,*)"**wrong size for xnode u component**"
                call exit_function
            end if
        end if
        
        if(present(du)) then
            if(size(du)==size(this_xnode%du)) then
                this_xnode%du=du
            else
                 write(msg_file,*)"**wrong size for xnode du component**"
                 call exit_function
            end if
        end if
        
        if(present(v)) then
            if(size(v)==size(this_xnode%v)) then
                this_xnode%v=v
            else
                write(msg_file,*)"**wrong size for xnode v component**"
                call exit_function
            end if
        end if
        
        if(present(dof)) then
            if(allocated(this_xnode%dof)) then
                if(size(dof)==size(this_xnode%dof)) then
                    this_xnode%dof=dof
                else
                    deallocate(this_xnode%dof)
                    allocate(this_xnode%dof(size(dof)))
                    this_xnode%dof=dof
                end if         
            else
                allocate(this_xnode%dof(size(dof)))
                this_xnode%dof=dof           
            end if
        end if
        
        if(present(ddof)) then
            ! check if ddof dimension matches dof dimension
            if(.not.allocated(this_xnode%dof)) then
                write(msg_file,*)"**dof must be allocated before ddof in update_xnode**"
                call exit_function
            else if(size(ddof)/=size(this_xnode%dof)) then
                write(msg_file,*)"**ddof size must be consistent with dof size in update_xnode**"
                call exit_function
            end if           
            ! update ddof values
            if(allocated(this_xnode%ddof)) then
                if(size(ddof)==size(this_xnode%ddof)) then
                    this_xnode%ddof=ddof
                else
                    deallocate(this_xnode%ddof)
                    allocate(this_xnode%ddof(size(ddof)))
                    this_xnode%ddof=ddof
                end if         
            else
                allocate(this_xnode%ddof(size(ddof)))
                this_xnode%ddof=ddof           
            end if
        end if

      end subroutine update_xnode 
      
      
      
      
      
      
           ! update a node
    subroutine export_xnode(this_xnode,x,u,du,v,dof,ddof)
      
      	type(xnode),intent(in) :: this_xnode
        real(kind=dp),optional,intent(out) :: x(:),u(:),du(:),v(:)
        real(kind=dp),allocatable,optional,intent(out) :: dof(:),ddof(:)
        
        
        if(present(x)) then
            if(size(x)==size(this_xnode%x)) then
                x=this_xnode%x
            else
                write(msg_file,*)"**wrong size for x component**"
                call exit_function
            end if
        end if
        
        if(present(u)) then
            if(size(u)==size(this_xnode%u)) then
                u=this_xnode%u
            else
                write(msg_file,*)"**wrong size for u component**"
                call exit_function
            end if
        end if
        
        if(present(du)) then
            if(size(du)==size(this_xnode%du)) then
                du=this_xnode%du
            else
                 write(msg_file,*)"**wrong size for du component**"
                 call exit_function
            end if
        end if
        
        if(present(v)) then
            if(size(v)==size(this_xnode%v)) then
                v=this_xnode%v
            else
                write(msg_file,*)"**wrong size for v component**"
                call exit_function
            end if
        end if
        
        if(present(dof)) then
            if(allocated(this_xnode%dof)) then
                allocate(dof(size(this_xnode%dof)))
                dof=this_xnode%dof
            end if
        end if
        
        if(present(ddof)) then
            if(allocated(this_xnode%ddof)) then
                allocate(ddof(size(this_xnode%ddof)))
                ddof=this_xnode%ddof
            end if
        end if
        

    end subroutine export_xnode 
      
      

    
    end module xnode_module