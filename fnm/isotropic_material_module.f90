    module isotropic_material_module
    use parameter_module
    
    implicit none
    private

    type,public :: isotropic_modulus
        real(kind=dp) :: E, nu ! elastic moduli
    end type
    
    type,public :: isotropic_strength
        real(kind=dp) :: Xt, Xc ! tensile and compressive strengths
    end type
    
    type,public :: isotropic_toughness
        real(kind=dp) :: GIc, GIIc, GIIIc ! mode I,II,III fracture toughness
    end type

    type,public :: isotropic_type
        type(isotropic_modulus),allocatable :: modulus(:)
        type(isotropic_strength),allocatable :: strength(:)
        type(isotropic_toughness),allocatable :: toughness(:)  
    end type
    
    
    interface empty
        module procedure empty_isotropic
    end interface
      
    interface update
        module procedure update_isotropic
    end interface
    
   
    public :: empty,update
  


  
    contains
    
    
    
      subroutine empty_isotropic(this_isotropic)
      
      	type(isotropic_type),intent(inout) :: this_isotropic
        
        integer :: istat
      	
        if(allocated(this_isotropic%modulus)) then
            deallocate(this_isotropic%modulus,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_isotropic**"
        end if

        if(allocated(this_isotropic%strength)) then
            deallocate(this_isotropic%strength,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_isotropic**"
        end if
        
        if(allocated(this_isotropic%toughness)) then
            deallocate(this_isotropic%toughness,stat=istat)
            if(istat/=0) stop"**deallocation error in empty_isotropic**"
        end if

      end subroutine empty_isotropic
 


      subroutine update_isotropic(this_isotropic,modulus,strength,toughness)
      
      	type(isotropic_type),intent(inout) :: this_isotropic
        type(isotropic_modulus),optional,intent(in) :: modulus
        type(isotropic_strength),optional,intent(in) :: strength
        type(isotropic_toughness),optional,intent(in) :: toughness
           
        integer :: istat
         
        if(present(modulus)) then
            if(allocated(this_isotropic%modulus)) then
                this_isotropic%modulus(1)=modulus        
            else
                allocate(this_isotropic%modulus(1),stat=istat)
                if(istat/=0) stop"**allocation error in update_isotropic**"
                this_isotropic%modulus(1)=modulus            
            end if
        end if
        
        if(present(strength)) then
            if(allocated(this_isotropic%strength)) then
                this_isotropic%strength(1)=strength        
            else
                allocate(this_isotropic%strength(1),stat=istat)
                if(istat/=0) stop"**allocation error in update_isotropic**"
                this_isotropic%strength(1)=strength            
            end if
        end if
        
        if(present(toughness)) then
            if(allocated(this_isotropic%toughness)) then
                this_isotropic%toughness(1)=toughness       
            else
                allocate(this_isotropic%toughness(1),stat=istat)
                if(istat/=0) stop"**allocation error in update_isotropic**"
                this_isotropic%toughness(1)=toughness
            end if
        end if

      end subroutine update_isotropic   
      
      
    end module isotropic_material_module