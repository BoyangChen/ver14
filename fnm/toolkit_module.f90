    module toolkit_module
    use parameter_module
    
    implicit none
    
    contains
    
    
c********************************************************************************************
c******************** subroutine deemat_global **********************************************
c**** to transform d matrix from local to global coordinates ********************************
c********************************************************************************************
      subroutine kdeemat_global(nst,dee,deeg,theta)
      use modparam
c
      include 'aba_param.inc'
c     
      dimension dee(nst,nst), deeg(nst,nst)
c
      dimension dee2(nst,nst)
      integer i,j
c     initialize local array
      call kinitial(dee2,nst,nst)
c
      c=cos(pi*theta/halfcirc)
      s=sin(pi*theta/halfcirc)
c     d matrix in global coords stored first in local array dee2
      if (nst .eq. 3) then
        dee2(1,1) = c*c*c*c*dee(1,1) + two*c*c*s*s*(dee(1,2)
     &            + two*dee(3,3)) + s*s*s*s*dee(2,2)
        dee2(1,2) = s*s*c*c*(dee(1,1) + dee(2,2) - four*dee(3,3))
     &            + (s*s*s*s+c*c*c*c)*dee(1,2)
        dee2(2,1) = dee2(1,2)
        dee2(2,2) = s*s*s*s*dee(1,1) + two*c*c*s*s*(dee(1,2)
     &            + two*dee(3,3)) + c*c*c*c*dee(2,2)
        dee2(1,3) = s*c*(c*c*(dee(1,1) - dee(1,2) - two*dee(3,3))
     &            + s*s*(dee(1,2) - dee(2,2) + two*dee(3,3)))
        dee2(3,1) = dee2(1,3)
        dee2(2,3) = s*c*(s*s*(dee(1,1) - dee(1,2) - two*dee(3,3))
     &            + c*c*(dee(1,2) - dee(2,2) + two*dee(3,3)))
        dee2(3,2) = dee2(2,3)
        dee2(3,3) = c*c*s*s*(dee(1,1)+dee(2,2)-2*dee(1,2))
     &			  +(c*c-s*s)**2*dee(3,3)
      else
       write(6,*) 'no. of strains not supported for kdeemat_global!'
       call xit
      end if
c     pass values to deeg
      do i=1,nst
        do j=1,nst
            deeg(i,j)=dee2(i,j)
        end do
      end do
c
      return
      end subroutine kdeemat_global
c********************************************************************************************
c********************************************************************************************



c********************************************************************************************
c********************* subroutine kdeterminant **********************************************
c********* returns the determinant of a jacobian matrix	*************************************
c********************************************************************************************
      subroutine kdeterminant(ajacob,detj,ndim)
c
      include 'aba_param.inc'
c
      dimension ajacob(ndim,ndim)
c
      if (ndim .eq. 2) then
        detj= ajacob(1,1)*ajacob(2,2) - ajacob(1,2)*ajacob(2,1)
      else
        write(6,*) 'dimension not supported for kdeterminant!'
        call xit
      end if
c
      return
      end subroutine kdeterminant
c********************************************************************************************
c********************************************************************************************



c********************************************************************************************    
c********************* subroutine kbeemat ***************************************************
c******* strain-displacement matrix for infinitesimal deformation ***************************
c********************************************************************************************
c
      subroutine kbeemat(bee,gn,nnode,nst,ndofel)
      use modparam
c
      include 'aba_param.inc'
c
      dimension gn(nnode,*),bee(nst,ndofel)
c
      integer i,j,k,l,m
c
      if (nst .eq. 3) then
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
      else
       write(6,*) 'no. of strains not supported for kbeemat!'
       call xit        
      end if
c
      return
      end subroutine kbeemat
c********************************************************************************************
c********************************************************************************************
c
c
c
c********************************************************************************************
c********************* subroutine kjac_inv **************************************************
c************** inverts a jacobian matrix onto itself ***************************************
c********************************************************************************************
      subroutine kjac_inv(ajacob,detj,ndim)
      use modparam
c
      include 'aba_param.inc'
c
      dimension ajacob(ndim,ndim)
c
      dimension ajac(ndim,ndim)
      integer i,j
c
c	  initialize local matrices and vectors
      call kinitial(ajac,ndim,ndim)
c
      do i=1,ndim
        do j=1,ndim
          ajac(i,j)=ajacob(i,j)
        end do
      end do
c
      if(ndim .eq. 2) then
        ajacob(1,1)=ajac(2,2)
        ajacob(2,1)=-ajac(2,1)
        ajacob(1,2)=-ajac(1,2)
        ajacob(2,2)=ajac(1,1)
      else
       write(6,*) 'dimension not supported for kjac_inv!'
       call xit
      end if
c
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
c
      return
      end subroutine kjac_inv
c********************************************************************************************
c********************************************************************************************



c********************************************************************************************
c******************* subroutine ktransfer_strain ********************************************
c********** transfer strains from global to material coordinate systems *********************
c********************************************************************************************
      subroutine ktransfer_strain(nst,strain,theta)
      use modparam
c
      include 'aba_param.inc'
c
      dimension strain(nst)
c
      dimension strn(nst),t(nst,nst)
      integer i,j	  
c
c	  initialize local matrices and vectors
      call kinitial(strn,nst,1)
      call kinitial(t,nst,nst)
c
      c=cos(pi*theta/halfcirc)
      s=sin(pi*theta/halfcirc)
c
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
c
      do i=1,nst
        strain(i) = strn(i)
      end do
c
      return
      end subroutine ktransfer_strain
c********************************************************************************************
c********************************************************************************************



c********************************************************************************************
c******************* subroutine kunitv ******************************************************
c********** normalize vector and return its magnitude  **************************************
c********************************************************************************************
       subroutine kunitv(a,amag,ndim)
       use modparam
c
       include 'aba_param.inc'
c       
       dimension a(ndim)
       integer i
c
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
c        do nothing
        end if
c
       return
       end subroutine kunitv
c********************************************************************************************
c********************************************************************************************
    
    
    end module toolkit_module