! fortran test program

! include useful modules

        include 'parameter_module.f'
        include 'xgeometry_module.f'

        program test
        use parameter_module
        use xgeometry_module

        implicit none

! create geometrical quantities
        type(xedge) :: xedges(2)
        type(xnode) :: xnodes(2)

        integer :: i

        i=0




!! test xedge procedures
!
!        xedges(2)=xedge(end_nodes=[1,3],floating_nodes=[1],&
!        & current_status=0,parent=1,children=[1,2])
!
!
!        print*, 'xedge 2 end nodes:',xedges(2)%end_nodes
!        print*, 'xedge 2 floating nodes:',xedges(2)%floating_nodes
!        print*, 'xedge 2 current status:',xedges(2)%current_status
!        print*, 'xedge 2 parent:',xedges(2)%parent
!        print*, 'xedge 2 children:',xedges(2)%children
!
!        !call empty(xedges(2))
!
!        call update(xedges(2),current_status=1,end_nodes=[10,25],floating_nodes=[3,7],&
!        & children=[2,6],parent=8)
!
!        print*, 'xedge 2 end nodes:',xedges(2)%end_nodes
!        print*, 'xedge 2 floating nodes:',xedges(2)%floating_nodes
!        print*, 'xedge 2 current status:',xedges(2)%current_status
!        print*, 'xedge 2 parent:',xedges(2)%parent
!        print*, 'xedge 2 children:',xedges(2)%children


! test xnode procedures

        xnodes(2)=xnode(x=[1,2,3],u=0.1,&
        & du=0.0,v=1.0,dof=[1.5,2.0],ddof=[0.0])


        print*, 'xnode 2 x:',xnodes(2)%x
        print*, 'xnode 2 u:',xnodes(2)%u
        print*, 'xnode 2 du:',xnodes(2)%du
        print*, 'xnode 2 v:',xnodes(2)%v
        print*, 'xnode 2 dof:',xnodes(2)%dof
        print*, 'xnode 2 ddof:',xnodes(2)%ddof

        !call empty(xnodes(2))

        call update(xnodes(2),x=[one,one,one],u=[one,one,one],&
        & du=[zero,zero,zero],v=[zero,one,zero],dof=[ten,ten],ddof=[one,one])
        ! must be exactly kind=dp, otherwise won't compile

        print*, 'xnode 2 x:',xnodes(2)%x
        print*, 'xnode 2 u:',xnodes(2)%u
        print*, 'xnode 2 du:',xnodes(2)%du
        print*, 'xnode 2 v:',xnodes(2)%v
        print*, 'xnode 2 dof:',xnodes(2)%dof
        print*, 'xnode 2 ddof:',xnodes(2)%ddof


        end program test
