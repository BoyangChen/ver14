    module tri_element_module
    use paramter_module
    use material_module, only: material,export ! no update or empty allowed
    use integration_point_module ! integration point type
    use toolkit_module
    use global_lib_module, only: lib_tri, lib_node, lib_mat
    
    implicit none
    private
    
    integer, parameter :: ndim=2, nst=3, nnode=3, nig=1, ndof=ndim*nnode ! constants for type tri_element 
    
    
    type, public :: tri_element 
        private
        
        integer :: connec(nnode)=0 ! node indices in their global arrays
        integer :: matkey=0 ! material index in the global material arrays
        type(integration_point) :: ig_point(nig) ! x, xi, weight, stress, strain, sdv; initialize in prepare procedure
        real(kind=dp) :: K_matrix(ndof,ndof)=zero, F_vector(ndof)=zero ! k matrix and f vector
        logical :: PlaneStrain=.false.
        
        ! below are optional terms 
        
        real(kind=dp),allocatable :: rsdv(:)
        integer,allocatable :: isdv(:)
        logical,allocatable :: lsdv(:)
        
    end type
    
    
    
    
    interface empty
        module procedure empty_tri_element
    end interface
    
    interface prepare
        module procedure prepare_tri_element
    end interface
    
    interface integrate
        module procedure integrate_tri_element
    end interface
    
    


    public :: empty,prepare,integrate
    
    
    
    
    contains
    
    
    ! this subroutine is used to format the element for use
    ! it is used in the initialize_lib_elem procedure in the lib_elem module
    subroutine empty_tri_element(elem)
    
        type(tri_element),intent(out) ::elem
        
        integer :: i
        i=0
        
        elem%connec=0
        elem%matkey=0
        do i=1,nig
            call empty(elem%ig_point(i))
        end do
        elem%K_matrix=zero   
        elem%F_vector=zero       
        elem%PlaneStrain=.false. ! default value      
        if(allocated(elem%avg_rsdv)) deallocate(elem%avg_rsdv)       
        if(allocated(elem%avg_isdv)) deallocate(elem%avg_isdv)
        if(allocated(elem%avg_lsdv)) deallocate(elem%avg_lsdv)
    
    end subroutine empty_tri_element
    
    
    
    
    ! this subroutine is used to prepare the connectivity and material lib index of the element
    ! it is used in the initialize_lib_elem procedure in the lib_elem module
    subroutine prepare_tri_element(elem,connec,matkey)
    
        type(tri_element),intent(inout) :: elem
        integer,intent(in) :: connec(nnode)
        integer,intent(in) :: matkey
        
        real(kind=dp) :: x(ndim),stress(nst),strain(nst)
        integer :: i
        x=zero;stress=zero;strain=zero
        i=0
        
        elem%connec=connec
        elem%matkey=matkey
        
        do i=1,nig
            call update(elem%ig_point(i),x=x,stress=stress,strain=strain)
        end do
    
    end subroutine prepare_tri_element
    
    
    
    
    
    ! the integration subroutine, updates K matrix, F vector, integration point stress and strain
    ! as well as all the solution dependent variables (sdvs) at intg points and element
    subroutine integrate_tri_element(elem)
    
        type(tri_element),intent(inout) :: elem  
        
        ! the rest are all local variables
        
        ! variables to be extracted from global arrays
        type(xnode) :: node(nnode) ! x, u, du, v, extra dof ddof etc
        type(material) :: mat ! matname, mattype and matkey to glb mattype array
        
        ! variables defined locally
        real(kind=dp) :: K_matrix(ndof,ndof), F_vector(ndof)
        real(kind=dp) :: igxi(ndim,nig),igwt(nig) ! ig point natural coords and weights
        real(kind=dp) :: coords(ndim,nnode) ! coordinates of the element nodes
        real(kind=dp) :: u(ndof) ! nodal disp. vector
        logical :: PlaneStrain ! as the name suggests, true for plane strain stiffness
        real(kind=dp) :: fn(nnode),dn(nnode,ndim) ! shape functions & their deriv. physical space
        real(kind=dp) :: jac(ndim,ndim),gn(nnode,ndim),detj ! jacobian & shape func. deriv. natural space
        real(kind=dp) :: bee(nst,ndof),beet(ndof,nst) ! b matrix and its transpose
        real(kind=dp) :: dee(nst,nst),deeg(nst,nst) ! d matrix in local & global coord. syst.
        real(kind=dp) :: btd(ndof,nst),btdb(ndof,ndof) ! b'*d & b'*d*b
        real(kind=dp) :: tmpstrain(nst),tmpstress(nst) ! temporary strain and stress arrays        
        real(kind=dp),allocatable :: xj(:),uj(:)! nodal vars extracted from glb lib_node array
        
        integer :: i,j,kig
        
        ! initialize variables
        i=0; j=0; kig=0
        do i=1,nnode
            call empty(node(i))
        end do
        call empty(mat)
        K_matrix=zero; F_vector=zero
        igxi=zero; igwt=zero
        coords=zero; u=zero
        PlainStrain=.false.
        
        ! copy nodes from global node array 
        node(:)=lib_node(elem%connec(:))
        
        ! assign values to local arrays from nodal values
        do j=1,nnode
        
            ! export useful values from nodes
            call export(node(j),x=xj,u=uj)
            
            ! assign values to coords matrix and u vector
            if(allocated(xj)) then
                coords(:,j)=xj(:)
            else
                write(msg_file,*)'WARNING: x not allocated for node:',elem%connec(j)
            end if
            
            if(allocated(uj)) then 
                u((j-1)*ndim+1:j*ndim)=uj(1:ndim)
            else
                write(msg_file,*)'WARNING: u not allocated for node:',elem%connec(j)
            end if
            
        end do
        
        ! copy material values from global material array
        mat=lib_mat(elm%matkey)
        
        ! copy plain strain variable from element
        PlainStrain=elem%PlainStrain
        
        ! update ig point xi and weight
        call tri_ig(igxi,igwt)
          
        
        
        !-calculate strain,stress,stiffness,sdv etc. at each int point
      	do kig=1,nig 
        
            ! empty relevant arrays for reuse
            fn=zero; dn=zero
            jac=zero; gn=zero; detj=zero
            bee=zero; beet=zero; dee=zero; deeg=zero
            btd=zero; btdb=zero
            tmpstrain=zero; tmpstress=zero
            
        
            call tri_shape(igxi,fn,dn,kig) !-to get shape matrix and derivatives
            jac=matmul(coords,dn) ! get jacobian           
     		call determinant(jac,detj,ndim) !-get det(jac)            
     		call jac_inv(jac,detj,ndim) ! invert jac onto itself           
            gn=matmul(dn,jac)           
     		call beemat(bee,gn,nnode,nst,ndof) !-calculate b matrix nst*ndof
            
            ! calculate global strains
        	do i=1,nst
                do j=1,ndof
                    tmpstrain(i) = tmpstrain(i)+bee(i,j)*u(j)
                end do
        	end do
            
            ! transfer strain to local coordinates
        	call ktransfer_strain(nst,tmpstrain,theta) !-strain now in local coords
            
            
            ! get D matrix dee accord. to material properties, and update intg point stresses
            select case(mat%mattype)
                case('isotropic')
                
                    ! calculate D matrix, update tmpstress
                    call ddsdde(lib_isotropic(mat%matkey),dee,PlaneStrain,tmpstrain,tmpstress) 
                    
                case('lamina')
                
                    ! calculate D matrix, update tmpstress
                    call ddsdde(lib_lamina(mat%matkey),dee,PlaneStrain,tmpstrain,tmpstress)
                    
                case default
                    write(msg_file,*) 'material type not supported for tri element!'
                    call exit_function
            end select
            
            ! get D matrix in global coordinates deeg
            call kdeemat_global(nst,dee,deeg,theta)
            
            ! calculate B' D B
            beet=transpose(bee)
            btd=matmul(beet,deeg)
            btdb=matmul(btd,bee)

            ! integrate and update K matrix
       		do i=1,ndof
          		do j=1,ndof
              		K_matrix(i,j) = K_matrix(i,j)+btdb(i,j)*detj*igwt(kig) !-gauss integration
          		end do
        	end do	
            
            
            
            
            ! update strains to ig point strain array
            call update(elem%ig_point(kig),strain=tmpstrain)
            
            ! update stress of this ig point
            call update(elem%ig_point(kig),stress=tmpstress)
            
       	end do !-looped over all int points. ig=nig
        
        F_vector=matmul(K_matrix,u) 
        
        ! update element K matrix and F vector
        elem%K_matrix=K_matrix
        elem%F_vector=F_vector
        
    
    end subroutine integrate_tri_element
    
    
    
    
    
    
    
    
    
    
    
    
    ! the rest are private subroutines
    
    
    
    
    
    

    
 
    subroutine tri_ig(xi,wt)

      real(kind=dp),intent(inout) :: xi(ndim,nig),wt(nig)
	
      if (ndim .eq. 2) then
        if (nig .eq. 1) then
            xi(1,1)= one_third
            xi(2,1)= one_third
            wt(1) = half
        else
            write(msg_file,*) 'no. of integration points incorrect for tri3_ig!'
            call xit
        end if
      else
        write(msg_file,*) 'dimension incorrect for tri3_ig!'
        call xit
      end if

      return
    end subroutine tri_intg
    
    
    
    subroutine tri_shape(igxi,f,df,k)
      
        real(kind=dp),intent(inout) :: f(nnode),df(nnode,ndim)
        real(kind=dp),intent(in) :: igxi(ndim,nig)
        integer,intent(in) :: k ! ig point number
        
        real(kind=dp) :: xi,eta ! local variables

        if (ndim .eq. 2) then
            xi=igxi(1,k)
            eta=igxi(2,k)
            if (nnode .eq. 3) then
                f(1)=one-xi-eta
                f(2)=xi
                f(3)=eta
                df(1,1)=-one
                df(2,1)=one
                df(3,1)=zero
                df(1,2)=-one
                df(2,2)=zero
                df(3,2)=one
            else
                write(msg_file,*) 'no. of nodes incorrect for ktri_shape!'
                call xit
            end if
        else
            write(msg_file,*) 'dimension incorrect for ktri_shape!'
            call xit
        end if
       return
    end subroutine tri_shape
    
    
    end module tri_element_module