    module toolkit_module
    use parameter_module
    
    implicit none
    
    contains
    
    
!********************************************************************************************
!                           function glb_dee
!               to transform d matrix from local to global coordinates 
!                           (for composite lamina)
!********************************************************************************************
      function glb_dee(dee,theta)
    
      real(kind=dp),intent(in)   :: dee(:,:), theta
      real(kind=dp)              :: glb_dee(size(dee(:,1)),size(dee(1,:)))

      ! local variables
      real(kind=dp) :: c, s
      integer       :: nst, nrow, ncol
      
      glb_dee=zero
      c=zero; s=zero
      nst=0; nrow=0; ncol=0

      c=cos(pi*theta/halfcirc)
      s=sin(pi*theta/halfcirc)
      
      nrow=size(dee(:,1))
      ncol=size(dee(1,:))
      
      if(nrow/=ncol) then
        write(msg_file,*)"error: d matrix must be square!"
        call exit_function
      else
        nst=nrow
      end if
      
      ! d matrix in global coords stored first in local array glb_dee
      if (nst == 3) then
        glb_dee(1,1) = c*c*c*c*dee(1,1) + two*c*c*s*s*(dee(1,2) &
     &            + two*dee(3,3)) + s*s*s*s*dee(2,2)
        glb_dee(1,2) = s*s*c*c*(dee(1,1) + dee(2,2) - four*dee(3,3)) &
     &            + (s*s*s*s+c*c*c*c)*dee(1,2)
        glb_dee(2,1) = glb_dee(1,2)
        glb_dee(2,2) = s*s*s*s*dee(1,1) + two*c*c*s*s*(dee(1,2) &
     &            + two*dee(3,3)) + c*c*c*c*dee(2,2)
        glb_dee(1,3) = s*c*(c*c*(dee(1,1) - dee(1,2) - two*dee(3,3)) &
     &            + s*s*(dee(1,2) - dee(2,2) + two*dee(3,3)))
        glb_dee(3,1) = glb_dee(1,3)
        glb_dee(2,3) = s*c*(s*s*(dee(1,1) - dee(1,2) - two*dee(3,3)) &
     &            + c*c*(dee(1,2) - dee(2,2) + two*dee(3,3)))
        glb_dee(3,2) = glb_dee(2,3)
        glb_dee(3,3) = c*c*s*s*(dee(1,1)+dee(2,2)-2*dee(1,2)) &
     &			  +(c*c-s*s)**2*dee(3,3)
      else if (nst == 6) then
        glb_dee(1,1) = c*c*c*c*dee(1,1) + two*c*c*s*s*(dee(1,2) &
     &            + two*dee(4,4)) + s*s*s*s*dee(2,2)
        glb_dee(1,2) = s*s*c*c*(dee(1,1) + dee(2,2) - four*dee(4,4)) &
     &            + (s*s*s*s+c*c*c*c)*dee(1,2)
        glb_dee(2,1) = glb_dee(1,2)
        glb_dee(2,2) = s*s*s*s*dee(1,1) + two*c*c*s*s*(dee(1,2) &
     &            + two*dee(4,4)) + c*c*c*c*dee(2,2)
        glb_dee(1,3) = c*c*dee(1,3) + s*s*dee(2,3)
        glb_dee(3,1) = glb_dee(1,3)
        glb_dee(1,4) = s*c*(c*c*(dee(1,1) - dee(1,2) - two*dee(4,4)) &
     &            + s*s*(dee(1,2) - dee(2,2) + two*dee(4,4)))
        glb_dee(4,1) = glb_dee(1,4)
        glb_dee(2,3) = s*s*dee(1,3) + c*c*dee(2,3)
        glb_dee(3,2) = glb_dee(2,3)
        glb_dee(2,4) = s*c*(s*s*(dee(1,1) - dee(1,2) - two*dee(4,4)) &
     &            + c*c*(dee(1,2) - dee(2,2) + two*dee(4,4)))
        glb_dee(4,2) = glb_dee(2,4)
        glb_dee(3,3) = dee(3,3)
        glb_dee(3,4) = c*s*(dee(1,3) - dee(2,3))
        glb_dee(4,3) = glb_dee(3,4)
        glb_dee(5,5) = c*c*dee(5,5) + s*s*dee(6,6)
        glb_dee(5,6) = c*s*(dee(6,6) - dee(5,5))
        glb_dee(6,5) = glb_dee(5,6)
        glb_dee(6,6) = s*s*dee(5,5) + c*c*dee(6,6)
        glb_dee(4,4) = s*s*c*c*(dee(1,1) - two*dee(1,2) + dee(2,2)) &
     &            + (s*s - c*c)*(s*s - c*c)*dee(4,4)
      
      else
       write(msg_file,*) 'error: no. of strains not supported for glb_dee function!'
       call exit_function
      end if

      end function glb_dee
!********************************************************************************************
!********************************************************************************************



!********************************************************************************************
!********************* function determinant **********************************************
!********* returns the determinant of a jacobian matrix	*************************************
!********************************************************************************************
      function determinant(jacob)
!
      real(kind=dp),intent(in) ::   jacob(:,:)
      real(kind=dp)            ::   determinant
      
      integer :: ndim, nshape(2)
      
      determinant=zero
      ndim=0; nshape=0
      
      nshape=shape(jacob)
      
      if(nshape(1)/=nshape(2)) then
        write(msg_file,*)"error: jacobian matrix must be square!"
        call exit_function
      else
        ndim=nshape(1)
      end if
!
      if (ndim .eq. 2) then
        determinant= jacob(1,1)*jacob(2,2) - jacob(1,2)*jacob(2,1)
      else if(ndim .eq. 3) then
        determinant= jacob(1,1)*(jacob(2,2)*jacob(3,3)-jacob(3,2)*jacob(2,3))
        determinant= determinant- jacob(1,2)*(jacob(2,1)*jacob(3,3)-jacob(3,1)*jacob(2,3))
        determinant= determinant+ jacob(1,3)*(jacob(2,1)*jacob(3,2)-jacob(3,1)*jacob(2,2))
      else
        write(msg_file,*) "error:dimension not supported for determinant function!"
        call exit_function
      end if
!
      return
      end function determinant
!********************************************************************************************
!********************************************************************************************



!********************************************************************************************    
!********************* function beemat ***************************************************
!******* strain-displacement matrix for infinitesimal deformation ***************************
!********************************************************************************************
! convention of order: eps1, eps2, eps3, gamma12, gamma13, gamma23
!
      function beemat(gn)

      real(kind=dp),intent(in)  :: gn(:,:)
      real(kind=dp) :: beemat(size(gn(1,:))*(size(gn(1,:))+1)/2, size(gn(1,:))*size(gn(:,1)))

      ! local variables  
      real(kind=dp) :: x, y, z
      integer :: nnode, ndim, nst, ndof
      integer :: k,l,m,n
      
      beemat=zero
      x=zero; y=zero; z=zero
      nnode=0; ndim=0; nst=0; ndof=0
      k=0; l=0; m=0; n=0
      
      nnode=size(gn(:,1))
      ndim=size(gn(1,:))
      nst=ndim*(ndim+1)/2

      if (nst == 3) then
        do m=1,nnode
          k= 2*m
          l=k-1
          x=gn(m,1)
          y=gn(m,2)
          beemat(1,l)= x
          beemat(3,k)= x
          beemat(2,k)= y
          beemat(3,l)= y
        end do
      else if (nst == 6) then
        do m=1,nnode
          n= 3*m
          k=n-1
          l=k-1
          x=gn(m,1)
          y=gn(m,2)
          z=gn(m,3)
          beemat(1,l)=x
          beemat(4,k)=x
          beemat(5,n)=x
          beemat(2,k)=y
          beemat(4,l)=y
          beemat(6,n)=y
          beemat(3,n)=z
          beemat(5,l)=z
          beemat(6,k)=z
        end do
      else
       write(msg_file,*) 'no. of strains not supported for kbeemat!'
       call exit_function  
      end if

      return
      end function beemat
!********************************************************************************************
!********************************************************************************************
!
!
!
!********************************************************************************************
!********************* subroutine kjac_inv **************************************************
!************** inverts a jacobian matrix onto itself ***************************************
!********************************************************************************************
      function inverse(jacob,detj)
!
      real(kind=dp),intent(in)          :: jacob(:,:)
      real(kind=dp),optional,intent(in) :: detj  
      real(kind=dp)                     :: inverse(size(jacob(:,1)),size(jacob(1,:)))

      real(kind=dp) :: det
      integer :: ndim, nshape(2)
      
      inverse=zero
      det=zero
      ndim=0; nshape=0
      
      nshape=shape(jacob)
      
      if(nshape(1)/=nshape(2)) then
        write(msg_file,*)"error: jacobian matrix must be square!"
        call exit_function
      else
        ndim=nshape(1)
      end if  

      if(ndim == 2) then
        inverse(1,1)=jacob(2,2)
        inverse(2,1)=-jacob(2,1)
        inverse(1,2)=-jacob(1,2)
        inverse(2,2)=jacob(1,1)
      else if(ndim == 3) then
        inverse(1,1)=jacob(2,2)*jacob(3,3)-jacob(3,2)*jacob(2,3)
        inverse(2,1)=jacob(3,1)*jacob(2,3)-jacob(2,1)*jacob(3,3)
        inverse(3,1)=jacob(2,1)*jacob(3,2)-jacob(3,1)*jacob(2,2)
        inverse(1,2)=jacob(3,2)*jacob(1,3)-jacob(1,2)*jacob(3,3)
        inverse(2,2)=jacob(1,1)*jacob(3,3)-jacob(3,1)*jacob(1,3)
        inverse(3,2)=jacob(3,1)*jacob(1,2)-jacob(1,1)*jacob(3,2)
        inverse(1,3)=jacob(1,2)*jacob(2,3)-jacob(2,2)*jacob(1,3)
        inverse(2,3)=jacob(2,1)*jacob(1,3)-jacob(1,1)*jacob(2,3)
        inverse(3,3)=jacob(1,1)*jacob(2,2)-jacob(2,1)*jacob(1,2)
      else
       write(msg_file,*) "error: dimension not yet supported for inverse!"
       call exit_function
      end if
!
      if(present(detj)) then
        det=detj
      else
        det=determinant(jacob)
      end if
      
      if(det .gt. tiny(one)) then
        inverse=inverse/det
      else
        write(msg_file,*) "error: zero or negative detj in inverse!"
        call exit_function
      end if

      end function inverse
!********************************************************************************************
!********************************************************************************************



!********************************************************************************************
!******************* subroutine ktransfer_strain ********************************************
!********** transfer strains from global to local coordinate systems *********************
!                   (for composite lamina)
!********************************************************************************************
      function lcl_strain(strain,theta)

      real(kind=dp),intent(in)  :: strain(:), theta
      real(kind=dp)             :: lcl_strain(size(strain))

      real(kind=dp) :: c, s, t(size(strain),size(strain))
      integer :: nst
      
      lcl_strain=zero
      c=zero; s=zero; t=zero
      nst=0
      
      nst=size(strain)

      c=cos(pi*theta/halfcirc)
      s=sin(pi*theta/halfcirc)
!
      if (nst == 3) then
        t(1,1)=c*c
        t(1,2)=s*s
        t(1,3)=c*s
        t(2,1)=s*s
        t(2,2)=c*c
        t(2,3)=-c*s
        t(3,1)=-two*c*s
        t(3,2)=two*c*s
        t(3,3)=c*c-s*s
      else if (nst == 6) then
        t(1,1)=c*c
        t(1,2)=s*s
        t(1,4)=c*s
        t(2,1)=s*s
        t(2,2)=c*c
        t(2,4)=-c*s
        t(3,3)=one
        t(4,1)=-two*c*s
        t(4,2)=two*c*s
        t(4,4)=c*c-s*s
        t(5,5)=c
        t(5,6)=-s
        t(6,5)=s
        t(6,6)=c
      else
       write(msg_file,*) 'no. of strains not supported for lcl_strain!'
       call exit_function
      end if
      
      lcl_strain=matmul(t,strain)

      end function lcl_strain
!********************************************************************************************
!********************************************************************************************



!********************************************************************************************
!******************* subroutine normalize ***************************************************
!********** normalize vector and return its magnitude  **************************************
!********************************************************************************************
    subroutine normalize(a,amag)
       
        real(kind=dp), intent(inout) :: a(:)
        real(kind=dp), optional, intent(out)   :: amag
       
        real(dp) :: amag2
        
        if(present(amag)) amag=zero
        amag2=zero
       
        amag2=sqrt(dot_product(a,a))
        
        if(amag2 .gt. tiny(one)) then
            a=a/amag2
        else
            write(msg_file,*) 'warning:zero vector for kunitv!'
!           do nothing
        end if
        
        if(present(amag)) amag=amag2

       return
    end subroutine normalize
!********************************************************************************************
!********************************************************************************************


    function CrossProduct3D(a, b)
      real(dp), intent(in)  :: a(3), b(3)
      real(dp)              :: CrossProduct3D(3)

      CrossProduct3D(1) = a(2) * b(3) - a(3) * b(2)
      CrossProduct3D(2) = a(3) * b(1) - a(1) * b(3)
      CrossProduct3D(3) = a(1) * b(2) - a(2) * b(1)
    end function CrossProduct3D
    
    
    end module toolkit_module