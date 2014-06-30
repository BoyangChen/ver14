    module toolkit_module
    use parameter_module
    
    implicit none
    
    contains
    
    
!********************************************************************************************
!******************** subroutine deemat_global **********************************************
!**** to transform d matrix from local to global coordinates ********************************
!********************************************************************************************
      subroutine kdeemat_global(nst,dee,deeg,theta)
      use modparam
!
      include 'aba_param.inc'
!     
      dimension dee(nst,nst), deeg(nst,nst)
!
      dimension dee2(nst,nst)
      integer i,j
!     initialize local array
      call kinitial(dee2,nst,nst)
!
      c=cos(pi*theta/halfcirc)
      s=sin(pi*theta/halfcirc)
!     d matrix in global coords stored first in local array dee2
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
!     pass values to deeg
      do i=1,nst
        do j=1,nst
            deeg(i,j)=dee2(i,j)
        end do
      end do
!
      return
      end subroutine kdeemat_global
!********************************************************************************************
!********************************************************************************************



!********************************************************************************************
!********************* subroutine kdeterminant **********************************************
!********* returns the determinant of a jacobian matrix	*************************************
!********************************************************************************************
      subroutine kdeterminant(ajacob,detj,ndim)
!
      include 'aba_param.inc'
!
      dimension ajacob(ndim,ndim)
!
      if (ndim .eq. 2) then
        detj= ajacob(1,1)*ajacob(2,2) - ajacob(1,2)*ajacob(2,1)
      else
        write(6,*) 'dimension not supported for kdeterminant!'
        call xit
      end if
!
      return
      end subroutine kdeterminant
!********************************************************************************************
!********************************************************************************************



!********************************************************************************************    
!********************* subroutine kbeemat ***************************************************
!******* strain-displacement matrix for infinitesimal deformation ***************************
!********************************************************************************************
!
      subroutine kbeemat(bee,gn,nnode,nst,ndofel)
      use modparam
!
      include 'aba_param.inc'
!
      dimension gn(nnode,*),bee(nst,ndofel)
!
      integer i,j,k,l,m
!
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
!
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