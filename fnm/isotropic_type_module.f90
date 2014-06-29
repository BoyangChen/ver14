    module isotropic_type_module
    use parameter_module
    
    implicit none
    private

    type,public :: isotropic_modulus
        private
        real(kind=dp) :: E, nu ! elastic moduli
    end type
    
    type,public :: isotropic_strength
        private
        real(kind=dp) :: Xt, Xc ! tensile and compressive strengths
    end type
    
    type,public :: isotropic_toughness
        private
        real(kind=dp) :: GIc, GIIc, GIIIc ! mode I,II,III fracture toughness
    end type

    type,public :: isotropic_type
        private
        type(isotropic_modulus):: modulus
        type(isotropic_strength) :: strength
        type(isotropic_toughness):: toughness 
        logical :: modulus_active=.false.
        logical :: strength_active=.false.
        logical :: toughness_active=.false.
    end type
    
    
    interface empty
        module procedure empty_isotropic
    end interface
      
    interface update
        module procedure update_isotropic
    end interface
    
    interface ddsdde
        module procedure ddsdde_isotropic
    end interface
    
   
    public :: empty,update
  


  
    contains
    
    
    
      subroutine empty_isotropic(this_isotropic)
      
      	type(isotropic_type),intent(out) :: this_isotropic
        
      	
        this_isotropic%modulus_active=.false.
        this_isotropic%strength_active=.false.
        this_isotropic%toughness_active=.false.

      end subroutine empty_isotropic
 


      subroutine update_isotropic(this_isotropic,modulus,strength,toughness)
      
      	type(isotropic_type),intent(inout) :: this_isotropic
        type(isotropic_modulus),optional,intent(in) :: modulus
        type(isotropic_strength),optional,intent(in) :: strength
        type(isotropic_toughness),optional,intent(in) :: toughness
        
         
        if(present(modulus)) then
                this_isotropic%modulus=modulus  
                this_isotropic%modulus_active=.true.
        end if
        
        if(present(strength)) then
                this_isotropic%strength=strength
                this_isotropic%strength_active=.true.
        end if
        
        if(present(toughness)) then
                this_isotropic%toughness=toughness
                this_isotropic%toughness_active=.true.
        end if

      end subroutine update_isotropic  



!********************************************************************************************
!************************ subroutine ddsdde ************************************************
!***** returns the tangent stiffness matrix of the material ******************************
!********************************************************************************************
      subroutine ddsdde_isotropic(this_mat,dee,PlaneStrain,strain,stress,rsdv,isdv,lsdv)

        type(isotropic_type),intent(in) :: this_mat
        real(kind=dp),intent(out) :: dee(:,:)
        real(kind=dp),optional,intent(out) :: stress(:)
        real(kind=dp),optional,intent(in) :: strain(:)
        real(kind=dp),allocatable,optional,intent(inout) :: rsdv(:)
        integer,allocatable,optional,intent(inout) :: isdv(:)
        logical,allocatable,optional,intent(inout) :: lsdv(:)
        logical,optional,intent(in) :: PlaneStrain
        
        ! local variables
        real(kind=dp) :: E,nu,G,del
        integer :: nst
        
        ! initialize variables
        dee=zero
        E=zero; nu=zero; G=zero; del=zero
        nst=0 
        
        ! find no. of strains
        nst=size(dee(:,1))
        
        if(.not.this_mat%modulus_active) then
            write(msg_file,*) 'isotropic material modulus undefined!'
            call exit_function
        end if
        
        if(this_mat%strength_active) write(msg_file,*) 'isotropic failure criterion not yet supported!'
        
        if(this_mat%toughness_active) write(msg_file,*) 'isotropic damage evolution not yet supported!'
        
        ! calculate the linear elasticity stiffness matrix
        E=this_mat%modulus%E
        nu=this_mat%modulus%nu
        G=half*E/(one+v) ! shear modulus

        if (nst .eq. 3) then ! 2D problem
            if (PlainStrain) then
                del=(one+nu)*(one-2*nu)
                dee(1,1)=(one-nu)*E/del
                dee(2,2)=dee(1,1)
                dee(3,3)=G
                dee(1,2)=nu*E/del
                dee(2,1)=dee(1,2)   
            else ! plain stress
                del=one-nu**2
                dee(1,1)=E/del
                dee(2,2)=dee(1,1)
                dee(3,3)=G
                dee(1,2)=nu*E/del
                dee(2,1)=dee(1,2) 
            end if
        else if (nst .eq. 6) then ! 3D problem
            del=(one+nu)*(one-2*nu)
            dee(1,1)=(one-nu)*E/del
            dee(2,2)=dee(1,1)
            dee(3,3)=dee(1,1)
            dee(4,4)=G
            dee(5,5)=G
            dee(6,6)=G
            dee(1,2:3)=nu*E/del
            dee(2:3,1)=nu*E/del
            dee(2,3)=nu*E/del
            dee(3,2)=dee(2,3)
        else
            write(msg_file,*) 'no. of strains not supported for kdeemat!'
            call exit_function
        end if
        
        

      return
      end subroutine ddsdde_isotropic 
      
      
    end module isotropic_type_module