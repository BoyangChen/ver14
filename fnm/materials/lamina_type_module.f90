    module lamina_type_module
    use parameter_module
    
    implicit none
    private

    type,public :: lamina_modulus
        real(kind=dp) :: E1,E2,G12,G23,nu12,nu23 ! elastic moduli
    end type
    
    type,public :: lamina_strength
        real(kind=dp) :: Xt,Xc,Yt,Yc,Sl,St ! tensile, compressive, and shear strengths
    end type
    
    type,public :: lamina_toughness
        real(kind=dp) :: GfcT, GfcC ! fibre fracture toughness tensile and compressive
        real(kind=dp) :: GmcI, GmcII ! matrix fracture toughness mode I and II 
        real(kind=dp) :: eta ! mixed-mode law constant
    end type

    type,public :: lamina_type
        type(lamina_modulus) :: modulus
        type(lamina_strength) :: strength
        type(lamina_toughness) :: toughness
        logical :: modulus_active=.false.
        logical :: strength_active=.false.
        logical :: toughness_active=.false.
    end type
    
    
    interface empty
        module procedure empty_lamina
    end interface
      
    interface update
        module procedure update_lamina
    end interface
    
    interface extract
        module procedure extract_lamina
    end interface
    
    interface ddsdde
        module procedure ddsdde_lamina
    end interface
    
   
    public :: empty,update,extract,ddsdde
  


  
    contains
    
    
    
    
      subroutine empty_lamina(this_lamina)
      
      	type(lamina_type),intent(out) :: this_lamina
        
      	this_lamina%modulus=lamina_modulus(zero,zero,zero,zero,zero,zero)
        this_lamina%strength=lamina_strength(zero,zero,zero,zero,zero,zero)
        this_lamina%toughness=lamina_toughness(zero,zero,zero,zero,zero)
        
        this_lamina%modulus_active=.false.
        this_lamina%strength_active=.false.
        this_lamina%toughness_active=.false.

      end subroutine empty_lamina
 


      subroutine update_lamina(this_lamina,modulus,strength,toughness)
      
      	type(lamina_type),intent(inout) :: this_lamina
        type(lamina_modulus),optional,intent(in) :: modulus
        type(lamina_strength),optional,intent(in) :: strength
        type(lamina_toughness),optional,intent(in) :: toughness
        
         
        if(present(modulus)) then
                this_lamina%modulus=modulus  
                this_lamina%modulus_active=.true.
        end if
        
        if(present(strength)) then
                this_lamina%strength=strength
                this_lamina%strength_active=.true.
        end if
        
        if(present(toughness)) then
                this_lamina%toughness=toughness
                this_lamina%toughness_active=.true.
        end if

      end subroutine update_lamina  
      
      
      
      subroutine extract_lamina(this_lamina,modulus,strength,toughness, &
      & modulus_active,strength_active,toughness_active)
      
      	type(lamina_type),intent(in) :: this_lamina
        type(lamina_modulus),optional,intent(out) :: modulus
        type(lamina_strength),optional,intent(out) :: strength
        type(lamina_toughness),optional,intent(out) :: toughness
        logical,optional,intent(out) :: modulus_active,strength_active,toughness_active
        
         
        if(present(modulus)) modulus=this_lamina%modulus
        if(present(modulus_active)) modulus_active=this_lamina%modulus_active
        
        if(present(strength)) strength=this_lamina%strength
        if(present(strength_active)) strength_active=this_lamina%strength_active
               
        if(present(toughness)) toughness=this_lamina%toughness
        if(present(toughness_active)) toughness_active=this_lamina%toughness_active

      end subroutine extract_lamina 



!********************************************************************************************
!************************ subroutine ddsdde ************************************************
!***** returns the tangent stiffness matrix of the material ******************************
!********************************************************************************************
      subroutine ddsdde_lamina(this_mat,dee,strain,stress,PlaneStrain,rsdv,isdv,lsdv)

        type(lamina_type),                        intent(in)    :: this_mat
        real(kind=dp),                            intent(out)   :: dee(:,:)
        real(kind=dp),                  optional, intent(out)   :: stress(:)
        real(kind=dp),                  optional, intent(in)    :: strain(:)
        real(kind=dp),  allocatable,    optional, intent(inout) :: rsdv(:)
        integer,        allocatable,    optional, intent(inout) :: isdv(:)
        logical,        allocatable,    optional, intent(inout) :: lsdv(:)
        logical,                        optional, intent(in)    :: PlaneStrain
        
        ! local variables
        real(kind=dp) :: E1,E2,E3,G12,G13,G23,nu12,nu13,nu23,nu21,nu31,nu32,del
        integer       :: nst
        
        ! initialize variables
        dee=zero; stress=zero
        E1=zero; E2=zero; E3=zero
        G12=zero; G13=zero; G23=zero 
        nu12=zero; nu13=zero; nu23=zero
        nu21=zero; nu31=zero; nu32=zero; del=zero
        nst=0 
        
        ! find no. of strains
        nst=size(dee(:,1))
        
        if(.not.this_mat%modulus_active) then
            write(msg_file,*) 'lamina material modulus undefined!'
            call exit_function
        end if
        
        if(this_mat%strength_active) write(msg_file,*) 'lamina failure criterion not yet supported!'
        
        if(this_mat%toughness_active) write(msg_file,*) 'lamina damage evolution not yet supported!'
        
        ! calculate the linear elasticity stiffness matrix
        E1=this_mat%modulus%E1
        E2=this_mat%modulus%E2; E3=E2
        nu12=this_mat%modulus%nu12; nu13=nu12
        nu23=this_mat%modulus%nu23
        G12=this_mat%modulus%G12; G13=G12
        G23=this_mat%modulus%G23
        
        nu21=E2/E1*nu12; nu31=nu21
        nu32=E3/E2*nu23

        del= one-nu12*nu21-nu13*nu31-nu23*nu32-two*nu21*nu32*nu13
        
        if (nst .eq. 3) then ! 2D problem
          if(PlaneStrain) then
            dee(1,1)= E1*(one-nu23*nu32)/del
            dee(1,2)= E1*(nu21+nu23*nu31)/del
            dee(2,2)= E2*(one-nu13*nu31)/del
            dee(2,1)= dee(1,2)
            dee(3,3)= G12
          else
            dee(1,1)= E1/(one-nu12*nu21)
            dee(1,2)= E1*nu21/(one-nu12*nu21)
            dee(2,2)= E2/(one-nu12*nu21)
            dee(2,1)= dee(1,2)
            dee(3,3)= G12
          end if
        else if (nst .eq. 6) then ! 3D problem
          del = del/E1/E2/E3
          dee(1,1)= (one-nu23*nu32)/E2/E3/del
          dee(1,2)= (nu21+nu23*nu31)/E2/E3/del
          dee(1,3)= (nu31+nu21*nu32)/E2/E3/del
          dee(2,1)= dee(1,2)
          dee(2,2)= (one-nu13*nu31)/E1/E3/del
          dee(2,3)= (nu32+nu12*nu31)/E1/E3/del
          dee(3,1)= dee(1,3)
          dee(3,2)= dee(2,3)
          dee(3,3)= (one-nu12*nu21)/E1/E2/del
          dee(4,4)= g12
          dee(5,5)= g13
          dee(6,6)= g23
        else
            write(msg_file,*) 'no. of strains not supported for kdeemat!'
            call exit_function
        end if

        
        stress=matmul(dee,strain)
        
        

      return
      end subroutine ddsdde_lamina 
      
      
    end module lamina_type_module