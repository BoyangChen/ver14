    module lamina_material_module
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
    
   
    public :: empty,update
  


  
    contains
    
    
    
      subroutine empty_lamina(this_lamina)
      
      	type(lamina_type),intent(inout) :: this_lamina
      	
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
      
      
    end module lamina_material_module