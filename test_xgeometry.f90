! fortran test program

! include useful modules

        include 'parameter_module.f'
        include 'geometry_module.f'
        include 'xgeometry_module.f'

        program test 
        use parameter_module
        use xgeometry_module

        implicit none
        
        !print*, "hello &
        ! world!"
        
! create geometrical quantities
        type(xedge) :: xedges(12)

        integer :: i

        i=0


        

! test xedge procedures

        ! initiation
        do i=1, size(xedges)
        call prepare(xedges(i))
        !print*, 'xedge',i,'end points:',xedges(i)%end_points
        !print*, 'xedge',i,'break points:',xedges(i)%break_points
        !print*, 'xedge',i,'edge status:',xedges(i)%edge_status
        !print*, 'xedge',i,'parent:',xedges(i)%parent
        !print*, 'xedge',i,'children:',xedges(i)%children
        end do
        
        ! implicit constructor
        call prepare(xedges(2),4,12)
        xedges(2)=xedge([13,9],[3,5,87,6],1,4,[2,3,98765]) 
        
        print*, 'xedge 2 end points:',xedges(2)%end_points
        print*, 'xedge 2 break points:',xedges(2)%break_points
        print*, 'xedge 2 edge status:',xedges(2)%edge_status
        print*, 'xedge 2 parent:',xedges(2)%parent
        print*, 'xedge 2 children:',xedges(2)%children
        
        
        
        
        end program test
