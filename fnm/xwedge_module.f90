    module xwedge_module
      use parameter_module

      implicit none
      private


      type, public :: xwedge ! breakable wedge
        integer :: end_nodes(6)
        integer :: edges(9)
        integer :: tris(2)
        integer :: quads(3)
        integer,allocatable :: floating_nodes(:)
        integer :: current_status
        integer :: parent
        integer,allocatable :: children(:)
      end type xwedge

      interface empty
        module procedure empty_xwedge
      end interface

      interface update
        module procedure update_xwedge
      end interface

      public :: empty,update



      contains




      ! empty a breakable wedge
      subroutine empty_xwedge(this_xwedge)

      	type(xwedge),intent(inout) :: this_xwedge

        integer :: istat

        this_xwedge%end_nodes(:)=0
        this_xwedge%edges(:)=0
        this_xwedge%tris(:)=0
        this_xwedge%quads(:)=0
        this_xwedge%current_status=0
        this_xwedge%parent=0

        if(allocated(this_xwedge%floating_nodes)) then
        deallocate(this_xwedge%floating_nodes,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xwedge**"
        end if

        if(allocated(this_xwedge%children)) then
        deallocate(this_xwedge%children,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xwedge**"
        end if

      end subroutine empty_xwedge




      ! update a breakable wedge
      subroutine update_xwedge(this_xwedge,current_status,end_nodes,&
      & edges,tris,quads,floating_nodes,parent,children)

      	type(xwedge),intent(inout) :: this_xwedge
        integer,optional,intent(in) :: current_status,parent
        integer,optional,intent(in) :: end_nodes(:),edges(:),tris(:),quads(:)
        integer,optional,intent(in) :: floating_nodes(:),children(:)

        integer :: istat

        if(present(current_status)) this_xwedge%current_status=current_status

        if(present(parent)) this_xwedge%parent=parent

        if(present(end_nodes)) then
            if(size(end_nodes)==size(this_xwedge%end_nodes)) then
                this_xwedge%end_nodes(:)=end_nodes(:)
            else
                stop"**wrong size for xwedge end_nodes component**"
            end if
        end if

        if(present(edges)) then
            if(size(edges)==size(this_xwedge%edges)) then
                this_xwedge%edges(:)=edges(:)
            else
                stop"**wrong size for xwedge edges component**"
            end if
        end if

        if(present(tris)) then
            if(size(tris)==size(this_xwedge%tris)) then
                this_xwedge%tris(:)=tris(:)
            else
                stop"**wrong size for xwedge tris component**"
            end if
        end if

        if(present(quads)) then
            if(size(quads)==size(this_xwedge%quads)) then
                this_xwedge%quads(:)=quads(:)
            else
                stop"**wrong size for xwedge quads component**"
            end if
        end if

        if(present(floating_nodes)) then
            if(allocated(this_xwedge%floating_nodes)) then
                if(size(floating_nodes)==size(this_xwedge%floating_nodes)) then
                    this_xwedge%floating_nodes(:)=floating_nodes(:)
                else
                    deallocate(this_xwedge%floating_nodes,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xwedge**"
                    allocate(this_xwedge%floating_nodes(size(floating_nodes)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xwedge**"
                    this_xwedge%floating_nodes(:)=floating_nodes(:)
                end if
            else
                allocate(this_xwedge%floating_nodes(size(floating_nodes)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xwedge**"
                this_xwedge%floating_nodes(:)=floating_nodes(:)
            end if
        end if

        if(present(children)) then
            if(allocated(this_xwedge%children)) then
                if(size(children)==size(this_xwedge%children)) then
                    this_xwedge%children(:)=children(:)
                else
                    deallocate(this_xwedge%children,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xwedge**"
                    allocate(this_xwedge%children(size(children)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xwedge**"
                    this_xwedge%children(:)=children(:)
                end if
            else
                allocate(this_xwedge%children(size(children)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xwedge**"
                this_xwedge%children(:)=children(:)
            end if
        end if

      end subroutine update_xwedge


    end module xwedge_module
