    module interface_type_module
    use parameter_module
    
    implicit none
    private


    type,public :: interface_modulus
        real(kind=dp) :: Dnn, Dtt, Dll ! penalty stiffness
    end type
    
    type,public :: interface_strength
        real(kind=dp) :: tau_nc, tau_tc, tau_lc ! strengths, t and l dir. values may be different
    end type
    
    type,public :: interface_toughness
        real(kind=dp) :: Gnc, Gtc, Glc ! toughness values, t and l dir. values may be different
        real(kind=dp) :: eta ! mixed-mode law component; eta_nt/nl/tl are the same; may be different in future
    end type
    

    type,public :: interface_type
        type(interface_modulus) :: modulus
        type(interface_strength) :: strength
        type(interface_toughness) :: toughness
        logical :: modulus_active=.false.
        logical :: strength_active=.false.
        logical :: toughness_active=.false.
    end type
    

    
    interface empty
        module procedure empty_interface
    end interface
      
    interface update
        module procedure update_interface
    end interface
    
   
    public :: empty,update
  


  
    contains
    
    
    
      subroutine empty_interface(this_interface)
      
      	type(interface_type),intent(inout) :: this_interface
        
        this_interface%modulus_active=.false.
        this_interface%strength_active=.false.
        this_interface%toughness_active=.false.
      	
      end subroutine empty_interface
 


      subroutine update_interface(this_interface,modulus,strength,toughness)
      
      	type(interface_type),intent(inout) :: this_interface
        type(interface_modulus),optional,intent(in) :: modulus
        type(interface_strength),optional,intent(in) :: strength
        type(interface_toughness),optional,intent(in) :: toughness
        
        if(present(modulus)) then
                this_interface%modulus=modulus  
                this_interface%modulus_active=.true.
        end if
        
        if(present(strength)) then
                this_interface%strength=strength
                this_interface%strength_active=.true.
        end if
        
        if(present(toughness)) then
                this_interface%toughness=toughness
                this_interface%toughness_active=.true.
        end if
        

      end subroutine update_interface   
      
      
!********************************************************************************************
!************************ subroutine ddsdde ************************************************
!***** returns the tangent stiffness matrix of the material ******************************
!********************************************************************************************
      subroutine ddsdde_interface(this_mat,dee,strain,stress,sdv)

        type(interface_type),       intent(in)  :: this_mat
        real(kind=dp),  optional,   intent(in)  :: strain(:)
        real(kind=dp),              intent(out) :: dee(:,:)
        real(kind=dp),  optional,   intent(out) :: stress(:)
        
        type(sdv_array), optional,  intent(inout) :: sdv

        
        ! local variables
        real(kind=dp) :: Dnn0, Dtt0, Dll0, Dnn, Dtt, Dll
        real(kind=dp) :: tau_nc, tau_tc, tau_lc
        real(kind=dp) :: Gnc, Gtc, Glc, eta
        integer :: nst
        
        ! initialize variables
        dee=zero; if(present(stress)) stress=zero ! intent(out) vars
        Dnn0=zero; Dtt0=zero; Dll0=zero
        Dnn=zero;  Dtt=zero;  Dll=zero
        tau_nc=zero; tau_tc=zero; tau_lc=zero
        Gnc=zero; Gtc=zero; Glc=zero; eta=zero
        nst=0
        
        ! find no. of strains
        nst=size(dee(:,1))
        
        if(.not.this_mat%modulus_active) then
            write(msg_file,*) 'interface material modulus undefined!'
            call exit_function
        end if
        
        ! extract the original linear elasticity stiffness
        Dnn0=this_mat%modulus%Dnn
        Dtt0=this_mat%modulus%Dtt
        Dll0=this_mat%modulus%Dll
        
        ! calculate damage
        if(this_mat%strength_active)
        
        if(this_mat%toughness_active)

        if (nst .eq. 2) then ! 2D problem
            dee(1,1)=Dnn
            dee(2,2)=Dtt
        else if (nst .eq. 3) then ! 3D problem
            dee(1,1)=Dnn
            dee(2,2)=Dtt
            dee(3,3)=Dll
        else
            write(msg_file,*) 'no. of strains not supported for interface ddsdde!'
            call exit_function
        end if
        
        stress=matmul(dee,strain)
        
        

      return
      end subroutine ddsdde_interface 
      
      
    end module interface_type_module