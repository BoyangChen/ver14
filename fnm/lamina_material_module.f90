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
        type(lamina_modulus),allocatable :: modulus(:)
        type(lamina_strength),allocatable :: strength(:)
        type(lamina_toughness),allocatable :: toughness(:)  
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
        
        integer :: istat
      	
        if(allocated(this_lamina%modulus)) then
            deallocate(this_lamina%modulus,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_lamina**"
        end if

        if(allocated(this_lamina%strength)) then
            deallocate(this_lamina%strength,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_lamina**"
        end if
        
        if(allocated(this_lamina%toughness)) then
            deallocate(this_lamina%toughness,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_lamina**"
        end if

      end subroutine empty_lamina
 


      subroutine update_lamina(this_lamina,modulus,strength,toughness)
      
      	type(lamina_type),intent(inout) :: this_lamina
        type(lamina_modulus),optional,intent(in) :: modulus
        type(lamina_strength),optional,intent(in) :: strength
        type(lamina_toughness),optional,intent(in) :: toughness
           
        integer :: istat
         
        if(present(modulus)) then
            if(allocated(this_lamina%modulus)) then
                this_lamina%modulus(1)=modulus        
            else
                allocate(this_lamina%modulus(1),stat=istat)
                if(istat/=0) stop"**allocation error in update_lamina**"
                this_lamina%modulus(1)=modulus            
            end if
        end if
        
        if(present(strength)) then
            if(allocated(this_lamina%strength)) then
                this_lamina%strength(1)=strength        
            else
                allocate(this_lamina%strength(1),stat=istat)
                if(istat/=0) stop"**allocation error in update_lamina**"
                this_lamina%strength(1)=strength            
            end if
        end if
        
        if(present(toughness)) then
            if(allocated(this_lamina%toughness)) then
                this_lamina%toughness(1)=toughness       
            else
                allocate(this_lamina%toughness(1),stat=istat)
                if(istat/=0) stop"**allocation error in update_lamina**"
                this_lamina%toughness(1)=toughness
            end if
        end if

      end subroutine update_lamina   
      
      
    end module lamina_material_module