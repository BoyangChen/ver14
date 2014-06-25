    module xbrick_module
      use parameter_module

      implicit none
      private


      type, public :: xbrick ! breakable brick
        integer :: end_nodes(8)
        integer :: edges(12)
        integer :: quads(6)
        integer,allocatable :: floating_nodes(:)
        integer :: current_status
        integer :: parent
        integer,allocatable :: children(:)
      end type xbrick

      interface empty
        module procedure empty_xbrick
      end interface

      interface update
        module procedure update_xbrick
      end interface

      public :: empty,update



      contains




      ! empty a breakable brick
      subroutine empty_xbrick(this_xbrick)

      	type(xbrick),intent(inout) :: this_xbrick

        integer :: istat

        this_xbrick%end_nodes(:)=0
        this_xbrick%edges(:)=0
        this_xbrick%quads(:)=0
        this_xbrick%current_status=0
        this_xbrick%parent=0

        if(allocated(this_xbrick%floating_nodes)) then
        deallocate(this_xbrick%floating_nodes,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xbrick**"
        end if

        if(allocated(this_xbrick%children)) then
        deallocate(this_xbrick%children,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xbrick**"
        end if

      end subroutine empty_xbrick




      ! update a breakable brick
      subroutine update_xbrick(this_xbrick,current_status,end_nodes,&
      & edges,quads,floating_nodes,parent,children)

      	type(xbrick),intent(inout) :: this_xbrick
        integer,optional,intent(in) :: current_status,parent
        integer,optional,intent(in) :: end_nodes(:),edges(:),quads(:)
        integer,optional,intent(in) :: floating_nodes(:),children(:)

        integer :: istat

        if(present(current_status)) this_xbrick%current_status=current_status

        if(present(parent)) this_xbrick%parent=parent

        if(present(end_nodes)) then
            if(size(end_nodes)==size(this_xbrick%end_nodes)) then
                this_xbrick%end_nodes(:)=end_nodes(:)
            else
                stop"**wrong size for xbrick end_nodes component**"
            end if
        end if

        if(present(edges)) then
            if(size(edges)==size(this_xbrick%edges)) then
                this_xbrick%edges(:)=edges(:)
            else
                stop"**wrong size for xbrick edges component**"
            end if
        end if

        if(present(quads)) then
            if(size(quads)==size(this_xbrick%quads)) then
                this_xbrick%quads(:)=quads(:)
            else
                stop"**wrong size for xbrick quads component**"
            end if
        end if

        if(present(floating_nodes)) then
            if(allocated(this_xbrick%floating_nodes)) then
                if(size(floating_nodes)==size(this_xbrick%floating_nodes)) then
                    this_xbrick%floating_nodes(:)=floating_nodes(:)
                else
                    deallocate(this_xbrick%floating_nodes,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xbrick**"
                    allocate(this_xbrick%floating_nodes(size(floating_nodes)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xbrick**"
                    this_xbrick%floating_nodes(:)=floating_nodes(:)
                end if
            else
                allocate(this_xbrick%floating_nodes(size(floating_nodes)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xbrick**"
                this_xbrick%floating_nodes(:)=floating_nodes(:)
            end if
        end if

        if(present(children)) then
            if(allocated(this_xbrick%children)) then
                if(size(children)==size(this_xbrick%children)) then
                    this_xbrick%children(:)=children(:)
                else
                    deallocate(this_xbrick%children,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xbrick**"
                    allocate(this_xbrick%children(size(children)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xbrick**"
                    this_xbrick%children(:)=children(:)
                end if
            else
                allocate(this_xbrick%children(size(children)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xbrick**"
                this_xbrick%children(:)=children(:)
            end if
        end if

      end subroutine update_xbrick


    end module xbrick_module
