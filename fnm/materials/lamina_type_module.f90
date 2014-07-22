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
    
    type,public :: lamina_matrixtoughness
        real(kind=dp) :: GmcI, GmcII ! matrix fracture toughness mode I and II 
        real(kind=dp) :: eta ! mixed-mode law constant
    end type
    
    type,public :: lamina_fibretoughness
        real(kind=dp) :: GfcT, GfcC ! fibre fracture toughness tensile and compressive
    end type

    type,public :: lamina_type
        type(lamina_modulus) :: modulus
        type(lamina_strength) :: strength
        type(lamina_matrixtoughness) :: matrixtoughness
        type(lamina_fibretoughness) :: fibretoughness
        logical :: modulus_active=.false.
        logical :: strength_active=.false.
        logical :: matrixtoughness_active=.false.
        logical :: fibretoughness_active=.false.
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
        this_lamina%matrixtoughness=lamina_matrixtoughness(zero,zero,zero)
        this_lamina%fibretoughness=lamina_fibretoughness(zero,zero)
        
        this_lamina%modulus_active=.false.
        this_lamina%strength_active=.false.
        this_lamina%matrixtoughness_active=.false.
        this_lamina%fibretoughness_active=.false.


      end subroutine empty_lamina
 


      subroutine update_lamina(this_lamina,modulus,strength,matrixtoughness,fibretoughness)
      
      	type(lamina_type),intent(inout) :: this_lamina
        type(lamina_modulus),optional,intent(in) :: modulus
        type(lamina_strength),optional,intent(in) :: strength
        type(lamina_matrixtoughness),optional,intent(in) :: matrixtoughness
        type(lamina_fibretoughness),optional,intent(in) :: fibretoughness
        
         
        if(present(modulus)) then
                this_lamina%modulus=modulus  
                this_lamina%modulus_active=.true.
        end if
        
        if(present(strength)) then
                this_lamina%strength=strength
                this_lamina%strength_active=.true.
        end if
        
        if(present(matrixtoughness)) then
                this_lamina%matrixtoughness=matrixtoughness
                this_lamina%matrixtoughness_active=.true.
        end if
        
        if(present(fibretoughness)) then
                this_lamina%fibretoughness=fibretoughness
                this_lamina%fibretoughness_active=.true.
        end if

      end subroutine update_lamina  
      
      
      
      subroutine extract_lamina(this_lamina,modulus,strength,matrixtoughness,fibretoughness, &
      & modulus_active,strength_active,matrixtoughness_active,fibretoughness_active)
      
      	type(lamina_type),intent(in) :: this_lamina
        type(lamina_modulus),optional,intent(out) :: modulus
        type(lamina_strength),optional,intent(out) :: strength
        type(lamina_matrixtoughness),optional,intent(out) :: matrixtoughness
        type(lamina_fibretoughness),optional,intent(out) :: fibretoughness
        logical,optional,intent(out) :: modulus_active,strength_active
        logical,optional,intent(out) :: matrixtoughness_active,fibretoughness_active
        
         
        if(present(modulus)) modulus=this_lamina%modulus
        if(present(modulus_active)) modulus_active=this_lamina%modulus_active
        
        if(present(strength)) strength=this_lamina%strength
        if(present(strength_active)) strength_active=this_lamina%strength_active
               
        if(present(matrixtoughness)) matrixtoughness=this_lamina%matrixtoughness
        if(present(matrixtoughness_active)) matrixtoughness_active=this_lamina%matrixtoughness_active
        
        if(present(fibretoughness)) fibretoughness=this_lamina%fibretoughness
        if(present(fibretoughness_active)) fibretoughness_active=this_lamina%fibretoughness_active

      end subroutine extract_lamina 



!********************************************************************************************
!************************ subroutine ddsdde ************************************************
!***** returns the tangent stiffness matrix of the material ******************************
!********************************************************************************************
      subroutine ddsdde_lamina(this_mat,dee,strain,stress,PlaneStrain,sdv,dfail)

        type(lamina_type),                        intent(in)    :: this_mat
        real(kind=dp),                            intent(out)   :: dee(:,:)
        real(kind=dp),                  optional, intent(out)   :: stress(:)
        real(kind=dp),                  optional, intent(in)    :: strain(:)
        logical,                        optional, intent(in)    :: PlaneStrain
        
        type(sdv_array),                optional, intent(inout) :: sdv
        real(kind=dp),                  optional,   intent(in)  :: dfail
        
        
        
        ! local variables
        real(kind=dp) :: E1,E2,E3,G12,G13,G23,nu12,nu13,nu23,nu21,nu31,nu32,del
        real(kind=dp) :: eps(size(dee(:,1)))  ! local replica of strain
        real(kind=dp) :: sig(size(dee(:,1)))  ! local replica of stress
        real(kind=dp) :: clength              ! characteristic element length
        real(kind=dp) :: dmax                 ! max. damage for smeared crack model
        integer       :: nst, fstat
        
        ! initialize intent(out) variables
        dee=zero; if(present(stress)) stress=zero
        ! initialize local variables
        E1=zero; E2=zero; E3=zero
        G12=zero; G13=zero; G23=zero 
        nu12=zero; nu13=zero; nu23=zero
        nu21=zero; nu31=zero; nu32=zero; del=zero
        eps=zero; sig=zero
        clength=zero
        dmax=zero
        nst=0; fstat=0
        
        ! find no. of strains
        nst=size(dee(:,1))
        
        if(.not.this_mat%modulus_active) then
            write(msg_file,*) 'lamina material modulus undefined!'
            call exit_function
        end if
        
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
          if (present(PlaneStrain).and.PlaneStrain) then
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

        
        ! if jump or sdv are not passed in, only linear elasticity can be done
        if((.not.present(strain)) .or. (.not.present(sdv))) then
            write(msg_file,*) 'WARNING: jump and sdv are not present in interface!'
            write(msg_file,*) 'Only linear elastic stiffness matrix can be calculated.' 
            return ! exit the program
        end if
        
       
        ! if strength is not present, give a warning and do linear elasticity with stress
        if(.not.this_mat%strength_active) then
            write(msg_file,*) 'WARNING: strength parameters are not present in interface!'
            write(msg_file,*) 'Only linear elastic stiffness matrix and stress can be calculated.' 
            ! update stress
            if(present(stress)) stress=matmul(dee,strain)
            return ! exit the program
        end if
        


        ! --------------------------------------------------------- !
        ! -- reaching here, strain, sdv and strength are present -- !
        ! -- failure criterion can be done                          !
        ! --------------------------------------------------------- !

             
        ! extract max. degradation at total failure
        if(present(dfail))  then
            dmax=dfail ! input parameter, valued at the coh. elem.
        else                
            dmax=one
        end if
        
        ! extract current values of failure status variable
        if(.not.allocated(sdv%i)) then 
            allocate(sdv%i(3)); sdv%i=0 ! 1st iteration
        end if
        fstat=sdv%i(1)  ! generic failure status
        ffstat=sdv%i(2) ! fibre failure status
        mfstat=sdv%i(3) ! matrix failure status
        
        ! matrix toughness parameters are not active, do only strength failure criterion
        if(.not.this_mat%fibretoughness_active) then
            write(msg_file,*)'fibre failure must include fibre toughness to ensure robust prediction!'
            call exit_function
        else
            call FibreCohesiveLaw()
            ! update sdv
            sdv%i(2)=ffstat
        end if

        ! matrix toughness parameters are not active, do only strength failure criterion
        if(.not.this_mat%matrixtoughness_active) then
            ! check and update fstat
            if(mfstat/=onset) then
                ! calculate stress based on jump
                ! dee is defined based on intact stiffness
                sig=matmul(dee,strain)               
                ! go through failure criterion and update fstat
                call MatrixFailureCriterion(sig,this_mat%strength,mfstat)
            end if
            ! update sdv
            sdv%i(3)=mfstat
        else
            ! call MatrixCohesiveLaw()
            write(msg_file,*)'matrix failure smeared crack model not supported! use FNM cohesive subelem instead'
            call exit_function
        end if
        
        ! update stress
        if(present(stress)) stress=sig
        
        ! update fstat
        fstat=max(ffstat,mfstat)
        sdv%i(1)=fstat
        
        ! exit program
        return
      end subroutine ddsdde_lamina 






      subroutine CohesiveLaw(sdv,Dee,sigma,strength,toughness,jump,dmax)
      
        type(sdv_array),        intent(inout)   :: sdv
        real(dp),               intent(inout)   :: Dee(:,:), sigma(:)
        type(interface_strength), intent(in)    :: strength
        type(interface_toughness),intent(in)    :: toughness
        real(dp),                 intent(in)    :: jump(:), dmax
      
        ! local variables
        real(dp) :: Dnn0, dm, u0, uf, Gnc, Gtc, Glc, Gsc, eta, findex, &
        & Gn, Gt, Gl, Gs, bk, Gmc, u_eff, T_eff, T0, dm2
        integer  :: nst, fstat

        ! initialize local variables
        Dnn0=zero; dm=zero; u0=zero; uf=zero; Gnc=zero; Gtc=zero
        Glc=zero;  Gsc=zero;  eta=zero; findex=zero
        Gn=zero;  Gt=zero; Gl=zero; Gs=zero; bk=zero
        Gmc=zero; u_eff=zero; T_eff=zero; T0=zero; dm2=zero
        nst=0; fstat=0
        
        ! extract Dnn0 and nst
        Dnn0=dee(1,1)
        nst=size(dee(:,1))
        
        ! extract current values of failure status variable
        if(.not.allocated(sdv%i)) then
            allocate(sdv%i(1)); sdv%i=0 ! 1st iteration
        end if
        fstat=sdv%i(1)
        
        
        ! --------------------------------------------------------- !
        ! if already failed, calculate directly Dee and Sigma and exit
        ! --------------------------------------------------------- !
        if(fstat==failed) then    ! already failed
            ! calculate penalty stiffness
            dee=dee*(one-dmax)
            if(jump(1)<zero) dee(1,1)=Dnn0 ! crack closes, no damage
            ! calculate stress
            sigma=matmul(dee,jump)
            ! exit program
            return
        end if
        
        
        
        ! --------------------------------------------------------- !
        ! the following assumes linear cohesive law
        ! other types of cohesive law can also be used in the future
        ! just need to put in a selection criterion and algorithm
        ! --------------------------------------------------------- ! 
        ! e.g.: select case (toughness%CLtype)
        !           case(0)
        !               linear
        !           case(1)
        !               exponential
        !           ...
        
        
        ! extract damage parameters of last converged iteration
        if(.not.allocated(sdv%r)) then ! 1st iteration
            allocate(sdv%r(3))
            sdv%r=zero
        end if
        dm=sdv%r(1)
        u0=sdv%r(2)
        uf=sdv%r(3)
        
        ! extract toughness parameters
        Gnc=toughness%Gnc
        Gtc=toughness%Gtc
        Glc=toughness%Glc
        eta=toughness%eta 
     
     
        ! check and update fstat and damage variables
        
        ! still intact
        if(fstat==intact) then
            ! calculate stress based on jump
            ! dee is defined based on intact stiffness
            sigma=matmul(dee,jump)
            
            ! go through failure criterion and update fstat
            call FailureCriterion(sigma,strength,fstat,findex)
            
            ! failure onset
            if(fstat==onset) then
            ! calculate u0, uf and go to next control
            
                ! normal and shear strain energy density
                Gn=half*max(zero,sigma(1))*max(zero,jump(1))
                if(nst==2)  then
                    Gt=half*sigma(2)*jump(2)
                    Gs=Gt
                else        
                    !Gs=half*(sigma(2)*jump(2)+sigma(3)*jump(3))
                    Gt=half*sigma(2)*jump(2)
                    Gl=half*sigma(3)*jump(3)
                    Gs=Gt+Gl
                end if
                
                ! BK ratio
                bk=Gs/(Gn+Gs)
                
                
                ! effective jump and traction
                if(nst==2)  then
                    u_eff=sqrt(max(zero,jump(1))**2+jump(2)**2)
                else        
                    u_eff=sqrt(max(zero,jump(1))**2+jump(2)**2+jump(3)**2)
                end if
                T_eff=two*(Gn+Gs)/u_eff
                
                ! effective jump and traction at failure onset, taking into acc. of overshoot
                u0=u_eff/sqrt(findex)
                T0=T_eff/sqrt(findex)
                
                ! mixed mode fracture toughness (BK formula)
                !Gsc=sqrt(Gtc**2+Glc**2) ! a quadratic avg of the two shear toughness
                if(Gs>tiny(one)) then
                    Gsc=Gtc*(Gt/Gs)+Glc*(Gl/Gs)
                else
                    Gsc=half*Gtc+half*Glc
                end if
                Gmc=Gnc+(Gsc-Gnc)*(bk**eta)
                
                ! effective jump at final failure
                uf=two*Gmc/T0
                
                
            else if(fstat==failed) then
            ! this is the case where strength is close to zero
                dm=dmax
                
            else ! fstat==intact
            ! fstat remains intact; dm, u0 and uf remain zero as initialized
                continue
            end if
        end if
        
        ! failure started
        if(fstat==onset) then
        
            ! calculate dm
            if(uf <= u0 + tiny(one)) then ! brittle failure
                dm=dmax
            else
                ! effective jump and traction
                if(nst==2)  then
                    u_eff=sqrt(max(zero,jump(1))**2+jump(2)**2)
                else        
                    u_eff=sqrt(max(zero,jump(1))**2+jump(2)**2+jump(3)**2)
                end if
                ! linear cohesive softening law 
                if(u_eff > tiny(one)) then
                    dm2=uf/u_eff*(u_eff-u0)/(uf-u0)
                    dm=max(dm,dm2) ! must be larger than last equilibrium dm
                end if
            end if
        
            ! check dm and update fstat
            if (dm>=dmax) then
                dm=dmax
                fstat=failed
            end if
        end if
     

        
        ! update D matrix
        dee=dee*(one-dm)
        if(jump(1)<zero) dee(1,1)=Dnn0 ! crack closes, no damage
        
        ! update stress
        sigma=matmul(dee,jump)
        
        ! update sdv
        sdv%i(1)=fstat
        sdv%r(1)=dm
        sdv%r(2)=u0
        sdv%r(3)=uf
        
      end subroutine CohesiveLaw
      
      
      subroutine FailureCriterion(sigma,strength,fstat,findex)
      
        real(dp),                   intent(in) :: sigma(:)
        type(interface_strength),   intent(in) :: strength
        integer,                   intent(out) :: fstat
        real(dp), optional,        intent(out) :: findex
      
        ! local variables
        integer     :: nst
        real(dp)    :: tau_nc, tau_tc, tau_lc, findex2
        
        ! initialize variables
        if(present(findex)) findex=zero                     ! intent (out)
        tau_nc=zero; tau_tc=zero; tau_lc=zero; findex2=zero ! local
        nst=0; fstat=0
        
        ! extract nst
        nst=size(sigma)
      
        ! extract strength parameters

        tau_nc=strength%tau_nc
        tau_tc=strength%tau_tc
        tau_lc=strength%tau_lc
        
        ! --------------------------------------------------------- !
        ! the following assumes quadratic stress criterion
        ! other criteria can also be used in the future
        ! just need to put in a selection criterion and algorithm
        ! --------------------------------------------------------- ! 
        ! e.g.: select case (strength%FC)
        !           case(0)
        !               quad. stress
        !           case(1)
        !               max. stress
        !           ...
        
        if(min(tau_nc,tau_tc,tau_lc) > tiny(one)) then
            if(nst==3) then 
            findex2=sqrt((max(sigma(1),zero)/tau_nc)**2 + (sigma(2)/tau_tc)**2 + (sigma(3)/tau_lc)**2)
            else
            findex2=sqrt((max(sigma(1),zero)/tau_nc)**2 + (sigma(2)/tau_tc)**2)
            end if
            
            if(findex2>=one) fstat=onset
        else
            ! strength close to zero == already failed
            fstat=failed
        end if
        
        if(present(findex)) findex=findex2
      
      end subroutine FailureCriterion
      
       
      
    end module lamina_type_module