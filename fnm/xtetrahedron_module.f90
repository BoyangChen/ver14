    module xtetrahedron_module
      use parameter_module

      implicit none
      private


      type, public :: xtetrahedron ! breakable tetrahedron
        integer :: end_nodes(4)
        integer :: edges(6)
        integer :: tris(4)
        integer,allocatable :: floating_nodes(:)
        integer :: current_status
        integer :: parent
        integer,allocatable :: children(:)
      end type xtetrahedron

      interface empty
        module procedure empty_xtetrahedron
      end interface

      interface update
        module procedure update_xtetrahedron
      end interface

      public :: empty,update



      contains




      ! empty a breakable tetrahedron
      subroutine empty_xtetrahedron(this_xtetrahedron)

      	type(xtetrahedron),intent(inout) :: this_xtetrahedron

        integer :: istat

        this_xtetrahedron%end_nodes(:)=0
        this_xtetrahedron%edges(:)=0
        this_xtetrahedron%tris(:)=0
        this_xtetrahedron%current_status=0
        this_xtetrahedron%parent=0

        if(allocated(this_xtetrahedron%floating_nodes)) then
        deallocate(this_xtetrahedron%floating_nodes,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xtetrahedron**"
        end if

        if(allocated(this_xtetrahedron%children)) then
        deallocate(this_xtetrahedron%children,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xtetrahedron**"
        end if

      end subroutine empty_xtetrahedron




      ! update a breakable tetrahedron
      subroutine update_xtetrahedron(this_xtetrahedron,current_status,end_nodes,&
      & edges,tris,floating_nodes,parent,children)

      	type(xtetrahedron),intent(inout) :: this_xtetrahedron
        integer,optional,intent(in) :: current_status,parent
        integer,optional,intent(in) :: end_nodes(:),edges(:),tris(:)
        integer,optional,intent(in) :: floating_nodes(:),children(:)

        integer :: istat

        if(present(current_status)) this_xtetrahedron%current_status=current_status

        if(present(parent)) this_xtetrahedron%parent=parent

        if(present(end_nodes)) then
            if(size(end_nodes)==size(this_xtetrahedron%end_nodes)) then
                this_xtetrahedron%end_nodes(:)=end_nodes(:)
            else
                stop"**wrong size for xtetrahedron end_nodes component**"
            end if
        end if

        if(present(edges)) then
            if(size(edges)==size(this_xtetrahedron%edges)) then
                this_xtetrahedron%edges(:)=edges(:)
            else
                stop"**wrong size for xtetrahedron edges component**"
            end if
        end if

        if(present(tris)) then
            if(size(tris)==size(this_xtetrahedron%tris)) then
                this_xtetrahedron%tris(:)=tris(:)
            else
                stop"**wrong size for xtetrahedron tris component**"
            end if
        end if

        if(present(floating_nodes)) then
            if(allocated(this_xtetrahedron%floating_nodes)) then
                if(size(floating_nodes)==size(this_xtetrahedron%floating_nodes)) then
                    this_xtetrahedron%floating_nodes(:)=floating_nodes(:)
                else
                    deallocate(this_xtetrahedron%floating_nodes,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xtetrahedron**"
                    allocate(this_xtetrahedron%floating_nodes(size(floating_nodes)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xtetrahedron**"
                    this_xtetrahedron%floating_nodes(:)=floating_nodes(:)
                end if
            else
                allocate(this_xtetrahedron%floating_nodes(size(floating_nodes)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xtetrahedron**"
                this_xtetrahedron%floating_nodes(:)=floating_nodes(:)
            end if
        end if

        if(present(children)) then
            if(allocated(this_xtetrahedron%children)) then
                if(size(children)==size(this_xtetrahedron%children)) then
                    this_xtetrahedron%children(:)=children(:)
                else
                    deallocate(this_xtetrahedron%children,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xtetrahedron**"
                    allocate(this_xtetrahedron%children(size(children)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xtetrahedron**"
                    this_xtetrahedron%children(:)=children(:)
                end if
            else
                allocate(this_xtetrahedron%children(size(children)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xtetrahedron**"
                this_xtetrahedron%children(:)=children(:)
            end if
        end if

      end subroutine update_xtetrahedron


    end module xtetrahedron_module
