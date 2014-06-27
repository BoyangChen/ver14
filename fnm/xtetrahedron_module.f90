    module xtetrahedron_module
      use parameter_module
      use subelement_module

      implicit none
      private

      integer,parameter :: nnode=4, nedge=6, ntri=4, nquad=0

      type, public :: tetrahedron ! breakable tetrahedron
        integer :: end_node(nnode)
      end type tetrahedron

      type, public :: xtetrahedron ! breakable tetrahedron
        integer :: end_node(nnode) ! cnc to glb node arrays for accessing glb real(vertex) node numbers
        integer :: edge(nedge) ! cnc to glb edge arrays for accessing status var. and glb flo node numbers
        integer :: tri(ntri) ! cnc to glb tri arrays for accessing glb flo node numbers assoc. with the tri face
        integer,allocatable :: flo_node(:) ! assigned to this volume, in addition to the flo nodes on edges and faces
        integer :: curr_status
        type(subelement),allocatable :: sub_elem(:) ! sub element connectivities
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

        this_xtetrahedron%end_node(:)=0
        this_xtetrahedron%edge(:)=0
        this_xtetrahedron%tri(:)=0
        this_xtetrahedron%curr_status=0

        if(allocated(this_xtetrahedron%flo_node)) then
        deallocate(this_xtetrahedron%flo_node,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_xtetrahedron**"
        end if

      end subroutine empty_xtetrahedron




      ! update a breakable tetrahedron
      subroutine update_xtetrahedron(this_xtetrahedron,curr_status,end_node,&
      & edge,tri,flo_node)

      	type(xtetrahedron),intent(inout) :: this_xtetrahedron
        integer,optional,intent(in) :: curr_status
        integer,optional,intent(in) :: end_node(:),edge(:),tri(:)
        integer,optional,intent(in) :: flo_node(:)

        integer :: istat

        if(present(curr_status)) this_xtetrahedron%curr_status=curr_status

        if(present(end_node)) then
            if(size(end_node)==size(this_xtetrahedron%end_node)) then
                this_xtetrahedron%end_node(:)=end_node(:)
            else
                stop"**wrong size for xtetrahedron end_node component**"
            end if
        end if

        if(present(edge)) then
            if(size(edge)==size(this_xtetrahedron%edge)) then
                this_xtetrahedron%edge(:)=edge(:)
            else
                stop"**wrong size for xtetrahedron edge component**"
            end if
        end if

        if(present(tri)) then
            if(size(tri)==size(this_xtetrahedron%tri)) then
                this_xtetrahedron%tri(:)=tri(:)
            else
                stop"**wrong size for xtetrahedron tri component**"
            end if
        end if

        if(present(flo_node)) then
            if(allocated(this_xtetrahedron%flo_node)) then
                if(size(flo_node)==size(this_xtetrahedron%flo_node)) then
                    this_xtetrahedron%flo_node(:)=flo_node(:)
                else
                    deallocate(this_xtetrahedron%flo_node,stat=istat)
                    if(istat/=0) stop"**deallocation error in update_xtetrahedron**"
                    allocate(this_xtetrahedron%flo_node(size(flo_node)),stat=istat)
                    if(istat/=0) stop"**reallocation error in update_xtetrahedron**"
                    this_xtetrahedron%flo_node(:)=flo_node(:)
                end if
            else
                allocate(this_xtetrahedron%flo_node(size(flo_node)),stat=istat)
                if(istat/=0) stop"**allocation error in update_xtetrahedron**"
                this_xtetrahedron%flo_node(:)=flo_node(:)
            end if
        end if

      end subroutine update_xtetrahedron


    end module xtetrahedron_module
