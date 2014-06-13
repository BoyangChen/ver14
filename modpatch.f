       module modpatch
        use modparam

        implicit none
        integer,parameter:: biply=0 ! set to zero to not use this module in the program
        real(kind=dp),parameter:: theta1=zero, theta2=ninety

        contains

        subroutine checkply(theta,theta1,theta2,xel,ndim,nnd)

            integer,intent(in):: ndim,nnd
            real(kind=dp),intent(in)::theta1,theta2
            real(kind=dp),intent(in):: xel(ndim,nnd)
            real(kind=dp),intent(inout):: theta

            if(biply.eq.1) then ! check for fibre orientation, above x-axis: theta1; below: theta2
                
                if(minval(xel(2,:)).ge.zero.and.
     &             maxval(xel(2,:)).gt.zero) then ! element above x-axis
                    theta=theta1
                else if(minval(xel(2,:)).lt.zero.and.
     &             maxval(xel(2,:)).le.zero) then ! element below x-axis
                    theta=theta2
                else
                   write(6,*)'something wrong in justforpatch!'
                   call xit
                end if

            end if

        end subroutine checkply

       end module modpatch
