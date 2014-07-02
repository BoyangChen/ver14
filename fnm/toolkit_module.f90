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
      real(kind=dp)              :: glb_dee(shape(dee))

      ! local variables
      real(kind=dp) :: c, s
      integer       :: nst
      
      c=zero; s=zero
      nst=0

      c=cos(pi*theta/halfcirc)
      s=sin(pi*theta/halfcirc)
      
      if(shape(dee)(1)/=shape(dee)(2)) then
        write(msg_file,*)"ERROR: D matrix must be square!"
        call exit_function
      else
        nst=shape(dee)(1)
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
      ELSE IF (NST == 6) THEN
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
       write(msg_file,*) 'no. of strains not supported for glb_dee function!'
       call exit_function
      end if

      end function glb_dee
!********************************************************************************************
!********************************************************************************************



!********************************************************************************************
!********************* function determinant **********************************************
!********* returns the determinant of a jacobian matrix	*************************************
!********************************************************************************************
      function determinant(ajacobi)
!
      real(kind=dp),intent(in) ::   ajacobi(:,:)
      real(kind=dp)            ::   determinant
      
      integer :: ndim
      
      ndim=0
      
      if(shape(ajacobi)(1)/=shape(ajacobi)(2)) then
        write(msg_file,*)"ERROR: jacobian matrix must be square!"
        call exit_function
      else
        ndim=shape(ajacobi)(1)
      end if
!
      if (ndim .eq. 2) then
        detj= ajacobi(1,1)*ajacobi(2,2) - ajacobi(1,2)*ajacobi(2,1)
      else if(ndim .eq. 3) then
        DetJ= Ajacobi(1,1)*(Ajacobi(2,2)*Ajacobi(3,3)-Ajacobi(3,2)*Ajacobi(2,3))
        DetJ= DetJ- Ajacobi(1,2)*(Ajacobi(2,1)*Ajacobi(3,3)-Ajacobi(3,1)*Ajacobi(2,3))
        DetJ= DetJ+ Ajacobi(1,3)*(Ajacobi(2,1)*Ajacobi(3,2)-Ajacobi(3,1)*Ajacobi(2,2))
      else
        write(msg_file,*) "ERROR:dimension not supported for determinant function!"
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

      !dimension gn(nnode,ndim),bee(nst,ndof)
      real(kind=dp),intent(in)  :: gn(:,:)
      real(kind=dp) :: beemat(size(gn(1,:))*(size(gn(1,:))+1)/2, size(gn(1,:))*size(gn(:,1)))

      ! local variables  
      real(kind=dp) :: x, y, z
      integer :: nnode, ndim, nst, ndof
      integer :: k,l,m,n
      
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
          bee(1,l)= x
          bee(3,k)= x
          bee(2,k)= y
          bee(3,l)= y
        end do
      else if (nst == 6) then
        do m=1,nnode
          n= 3*m
          k=n-1
          l=k-1
          x=gn(1,m)
          y=gn(2,m)
          z=gn(3,m)
          bee(1,l)=x
          bee(4,k)=x
          bee(5,n)=x
          bee(2,k)=y
          bee(4,l)=y
          bee(6,n)=y
          bee(3,n)=z
          bee(5,l)=z
          bee(6,k)=z
        end do
      else
       write(msg_file,*) 'no. of strains not supported for kbeemat!'
       call exit_function  
      end if

      return
      end subroutine kbeemat
!********************************************************************************************
!********************************************************************************************
!
!
!
!********************************************************************************************
!********************* subroutine kjac_inv **************************************************
!************** inverts a jacobian matrix onto itself ***************************************
!********************************************************************************************
      subroutine kjac_inv(ajacob,detj,ndim)
      use modparam
!
      include 'aba_param.inc'
!
      dimension ajacob(ndim,ndim)
!
      dimension ajac(ndim,ndim)
      integer i,j
!
!	  initialize local matrices and vectors
      call kinitial(ajac,ndim,ndim)
!
      do i=1,ndim
        do j=1,ndim
          ajac(i,j)=ajacob(i,j)
        end do
      end do
!
      if(ndim .eq. 2) then
        ajacob(1,1)=ajac(2,2)
        ajacob(2,1)=-ajac(2,1)
        ajacob(1,2)=-ajac(1,2)
        ajacob(2,2)=ajac(1,1)
      else
       write(6,*) 'dimension not supported for kjac_inv!'
       call xit
      end if
!
      if(detj .gt. zero) then
        do i=1,ndim
            do j=1,ndim
                ajacob(i,j)=ajacob(i,j)/detj
            end do
        end do
      else
       write(6,*) 'zero or negative detj in kjac_inv!'
       call xit
      end if
!
      return
      end subroutine kjac_inv
!********************************************************************************************
!********************************************************************************************



!********************************************************************************************
!******************* subroutine ktransfer_strain ********************************************
!********** transfer strains from global to material coordinate systems *********************
!********************************************************************************************
      subroutine ktransfer_strain(nst,strain,theta)
      use modparam
!
      include 'aba_param.inc'
!
      dimension strain(nst)
!
      dimension strn(nst),t(nst,nst)
      integer i,j	  
!
!	  initialize local matrices and vectors
      call kinitial(strn,nst,1)
      call kinitial(t,nst,nst)
!
      c=cos(pi*theta/halfcirc)
      s=sin(pi*theta/halfcirc)
!
      if (nst .eq. 3) then
        t(1,1)=c*c
        t(1,2)=s*s
        t(1,3)=c*s
        t(2,1)=s*s
        t(2,2)=c*c
        t(2,3)=-c*s
        t(3,1)=-two*c*s
        t(3,2)=two*c*s
        t(3,3)=c*c-s*s
      else
       write(6,*) 'no. of strains not supported for ktransfer_strain!'
       call xit
      end if
      call kmatrix_mul(t,strain,strn,nst,nst,1)
!
      do i=1,nst
        strain(i) = strn(i)
      end do
!
      return
      end subroutine ktransfer_strain
!********************************************************************************************
!********************************************************************************************



!********************************************************************************************
!******************* subroutine kunitv ******************************************************
!********** normalize vector and return its magnitude  **************************************
!********************************************************************************************
       subroutine kunitv(a,amag,ndim)
       use modparam
!
       include 'aba_param.inc'
!       
       dimension a(ndim)
       integer i
!
        amag=zero
        do i=1,ndim
         amag=amag+a(i)*a(i)
        end do
        amag=dsqrt(amag)
        if(amag .ne. zero) then
         do i=1,ndim
            a(i)=a(i)/amag
         end do
        else
         write(6,*) 'zero vector for kunitv!'
!        do nothing
        end if
!
       return
       end subroutine kunitv
!********************************************************************************************
!********************************************************************************************
    
    
    end module toolkit_module