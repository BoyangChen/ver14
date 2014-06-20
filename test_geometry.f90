! fortran test program

! include useful modules

        include 'parameter_module.f'
        include 'geometry_module.f'

        program test 
        use parameter_module
        use geometry_module

        implicit none
        
        !print*, "hello &
        ! world!"
        
! create geometrical quantities
        type(point):: points(8)
        type(edge) :: edges(12)
        type(triangle) :: triangles(4)
        type(quadrilateral) :: quadrilaterals(6)
        type(tetrahedron) :: tetrahedrons(3)
        type(wedge) :: wedges(3)
        type(brick) :: bricks(3)
        
        real(kind=dp) :: x(3)
        integer :: i
        
        x(:)=zero;i=0
        
! test point procedures

        ! initiation
        do i=1, size(points)
        !call points(i)%prepare
        call prepare(points(i))
        !print*, points(i)%x(:)
        end do
        
        ! implicit constructor  
        x(1)=one; x(2)=two; x(3)=three
     
        points(2)=point([two,two,two])   
        points(2)=point(zero)
        points(2)=point(one) 
        points(2)=point(eight)
        points(2)=point(x)
        !print*, 'point 2 coords:', points(2)%x(:)
        
! test edge procedures

        ! initiation
        do i=1, size(edges)
        !call edges(i)%prepare
        call prepare(edges(i))
        !print*, 'edge',i,'points:',edges(i)%points
        end do
        
        ! implicit constructor
        edges(2)=edge([13,9]) 
        !print*, 'edge 2 points:',edges(2)%points
        
! test triangle procedures

        ! initiation
        do i=1, size(triangles)
        !call triangles(i)%prepare
        !print*,triangles(i)%points
        !print*,triangles(i)%edges
        end do
        
        ! implicit constructor
        triangles(3)=triangle([1,2,3],0)
        !print*,triangles(3)%points
        !print*,triangles(3)%edges
        
! test quadrilateral procedures

        ! initiation
        do i=1, size(quadrilaterals)
        !call quadrilaterals(i)%prepare
        call prepare(quadrilaterals(i))
        !print*,quadrilaterals(i)%points
        !print*,quadrilaterals(i)%edges
        end do
        
        ! implicit constructor
        quadrilaterals(3)=quadrilateral([1,2,3,9],0)
        !print*,quadrilaterals(3)%points
        !print*,quadrilaterals(3)%edges

! test tetrahedron procedures

        ! initiation
        do i=1, size(tetrahedrons)
        !call tetrahedrons(i)%prepare
        !print*,tetrahedrons(i)%points
        !print*,tetrahedrons(i)%edges
        !print*,tetrahedrons(i)%triangles
        end do
        
        ! implicit constructor
        tetrahedrons(3)=tetrahedron(0,[1,5,2,3,7,4],[9,2,4,7])
        !print*,tetrahedrons(3)%points
        !print*,tetrahedrons(3)%edges
        !print*,tetrahedrons(3)%triangles


! test wedge procedures

        ! initiation
        do i=1, size(wedges)
        !call wedges(i)%prepare
        call prepare(wedges(i))
        !print*,wedges(i)%points
        !print*,wedges(i)%edges
        !print*,wedges(i)%triangles
        !print*,wedges(i)%quadrilaterals
        end do
        
        ! implicit constructor
        wedges(3)=wedge(0,[1,5,2,3,7,4,1,3,5],[9,2],[4,7,1])
        !print*,wedges(3)%points
        !print*,wedges(3)%edges
        !print*,wedges(3)%triangles
        !print*,wedges(3)%quadrilaterals
        

! test brick procedures

        ! initiation
        do i=1, size(bricks)
        !call bricks(i)%prepare
        call prepare(bricks(i))
        !print*,bricks(i)%points
        !print*,bricks(i)%edges
        !print*,bricks(i)%quadrilaterals
        end do
        
        ! implicit constructor
        bricks(3)=brick([1,6,3,4,9,6,2,4],[1,5,2,3,7,4,1,3,5,2,5,3],[9,2,4,7,1,3])
        !print*,bricks(3)%points
        !print*,bricks(3)%edges
        !print*,bricks(3)%quadrilaterals
        
        
        
        end program test
