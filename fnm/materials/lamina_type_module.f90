    module lamina_type_module
    use parameter_module
    
    implicit none
    private
    
    ! parameters
    integer, parameter :: mfailed=10, mfonset=5, ffailed=20, ffonset=15

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
 


      subroutine update_lamina(this_lamina,modulus,strength,fibretoughness,matrixtoughness)
      
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
      subroutine ddsdde_lamina(this_mat,clength,dee,strain,stress,PlaneStrain,sdv,dfail)

        type(lamina_type),                        intent(in)    :: this_mat
        real(kind=dp),                  optional, intent(in)    :: clength  ! characteristic element length
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
        real(kind=dp) :: df, dmax             ! fibre damage & max. damage for smeared crack model
        integer       :: nst, fstat, ffstat, mfstat
        
        ! initialize intent(out) variables
        dee=zero; if(present(stress)) stress=zero
        ! initialize local variables
        E1=zero; E2=zero; E3=zero
        G12=zero; G13=zero; G23=zero 
        nu12=zero; nu13=zero; nu23=zero
        nu21=zero; nu31=zero; nu32=zero; del=zero
        eps=zero; sig=zero
        df=zero; dmax=zero
        nst=0; fstat=0; ffstat=0; mfstat=0
        
        
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
        
        call deemat(E1,E2,E3,nu12,nu13,nu23,G12,G13,G23,dee)

        
        ! if jump or sdv are not passed in, only linear elasticity can be done
        if((.not.present(strain)) .or. (.not.present(sdv))) then
            write(msg_file,*) 'WARNING: strain and sdv are not present in lamina type!'
            write(msg_file,*) 'Only linear elastic stiffness matrix can be calculated.' 
            return ! exit the program
        end if
        
       
        ! if strength is not present, give a warning and do linear elasticity with stress
        if(.not.this_mat%strength_active) then
            write(msg_file,*) 'WARNING: strength parameters are not present in lamina type!'
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
            sig=matmul(dee,strain)
            call FibreCohesiveLaw(ffstat,sdv%r,sig,this_mat%strength, &
        &   this_mat%fibretoughness,strain,clength,dmax)
            ! update sdv
            sdv%i(2)=ffstat
            ! update ddsdde
            df=sdv%r(1)
            call deemat(E1,E2,E3,nu12,nu13,nu23,G12,G13,G23,dee,df)
            ! update stress
            sig=matmul(dee,strain)
        end if

        ! do only strength failure criterion
        if(this_mat%matrixtoughness_active) then
            write(msg_file,*)'matrix failure smeared crack model not supported! &
            & only failure criterion is assessed; use FNM cohesive subelem instead for matrix cracking'
        end if

        ! calculate stress based on jump
        ! dee is defined based on intact stiffness
        sig=matmul(dee,strain)
        ! check and update fstat
        if(mfstat<mfonset) then               
            ! go through failure criterion and update fstat
            call MatrixFailureCriterion(sig,this_mat%strength,mfstat)
        end if
        ! update sdv
        if(mfstat>=mfonset) sdv%i(3)=mfstat
        
        
        
        ! update stress
        if(present(stress)) stress=sig
        
        ! update fstat
        fstat=max(ffstat,mfstat)
        sdv%i(1)=fstat
        
        ! exit program
        return
      end subroutine ddsdde_lamina 




      subroutine deemat(E01,E02,E03,nu012,nu013,nu023,G012,G013,G023,dee,df,dm2,dm3,planestrain)
      
      real(dp), intent(in) :: E01,E02,E03,nu012,nu013,nu023,G012,G013,G023
      real(dp), intent(out) :: dee(:,:)
      real(dp), optional, intent(in) :: df, dm2, dm3
      logical,  optional, intent(in) :: planestrain
      
      !local var.
      real(dp) :: E1,E2,E3,nu12,nu13,nu23,G12,G13,G23
      real(dp) :: nu21, nu31, nu32, del, dm
      integer  :: nst
      
      dee=zero
      E1=zero; E2=zero; E3=zero
      G12=zero; G13=zero; G23=zero
      nu12=zero; nu13=zero; nu23=zero
      nu21=zero; nu31=zero; nu32=zero
      del=zero; dm=zero; nst=0
        
        E1=E01; E2=E02; E3=E03
        G12=G012; G13=G013; G23=G023
        nu12=nu012; nu13=nu013; nu23=nu023
        nu21=E2/E1*nu12; nu31=nu21; nu32=E3/E2*nu23
        
        
        if(present(df)) then
            E1=E1*(one-df)
            nu12=nu12*(one-df)
            nu13=nu13*(one-df)

            E1=max(E1,Kres)
        end if
        
        if(present(dm2)) then
            E2=E2*(one-dm2)
            nu21=nu21*(one-dm2)
            nu23=nu23*(one-dm2)
            G12=G12*(one-dm2)
            dm=max(dm,dm2)

            E2=max(E2,Kres)
            G12=max(G12,Kres)
        end if
        
        if(present(dm3)) then
            E3=E3*(one-dm3)
            nu31=nu31*(one-dm3)
            nu32=nu32*(one-dm3)
            G13=G13*(one-dm2)
            dm=max(dm,dm3)

            E3=max(E3,Kres)
            G13=max(G13,Kres)
        end if
            
        G23=G23*(one-dm)
        
        G23=max(G23,Kres)
        
        del= one-nu12*nu21-nu13*nu31-nu23*nu32-two*nu21*nu32*nu13
        
        ! find no. of strains
        nst=size(dee(:,1))
        
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
    end subroutine deemat




      subroutine FibreCohesiveLaw(fstat,sdvr,sigma,strength,fibretoughness,strain,clength,dmax)
      
        integer,                intent(inout)   :: fstat
        real(dp), allocatable,  intent(inout)   :: sdvr(:)
        real(dp),                 intent(in)    :: sigma(:),strain(:), clength, dmax 
        type(lamina_strength), intent(in)       :: strength
        type(lamina_fibretoughness),intent(in)  :: fibretoughness
        
      
        ! local variables
        real(dp) :: GfcT, GfcC, dm, u0, uf, T0, u_eff, T_eff, findex, dm2
        integer  :: nst
        
        
        ! --------------------------------------------------------- !
        ! if already failed, calculate directly Dee and Sigma and exit
        ! --------------------------------------------------------- !
        if(fstat==ffailed) then    ! already failed
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
        
        ! initialize local variables
        GfcT=zero; GfcC=zero
        dm=zero; u0=zero; uf=zero; T0=zero; findex=zero; dm2=zero
        u_eff=zero; T_eff=zero
        nst=0
        
        
        ! extract toughness parameters
        GfcT=fibretoughness%GfcT
        GfcC=fibretoughness%GfcC

        ! extract current values of failure status variable
        if(.not.allocated(sdvr)) then
            allocate(sdvr(3)); sdvr=zero  ! 1st iteration
        end if
        dm=sdvr(1)
        u0=sdvr(2)
        uf=sdvr(3) 
        
        ! extract nst
        nst=size(strain)
     
     
        ! check and update fstat and damage variables
        
        ! still intact
        if(fstat==intact) then
            
            ! go through failure criterion and update fstat
            call FibreFailureCriterion(sigma,strength,fstat,findex)
            
            ! failure onset
            if(fstat==ffonset) then
            ! calculate u0, uf and go to next control
                
                u_eff=abs(strain(1))*clength
                T_eff=abs(sigma(1))
                
                ! effective jump and traction at failure onset, taking into acc. of overshoot
                u0=u_eff/sqrt(findex)
                T0=T_eff/sqrt(findex)
                
                ! effective jump at final failure
                if(strain(1)>zero) then
                    uf=two*GfcT/T0
                else
                    uf=two*GfcC/T0
                end if
                
            else if(fstat==ffailed) then
            ! this is the case where strength is close to zero
                dm=dmax
                
            else ! fstat==intact
            ! fstat remains intact; dm, u0 and uf remain zero as initialized
                continue
            end if
        end if
        
        ! failure started
        if(fstat==ffonset) then
        
            ! calculate dm
            if(uf <= u0 + tiny(one)) then ! brittle failure
                dm=dmax
            else
                ! effective jump and traction
                u_eff=abs(strain(1))*clength
                ! linear cohesive softening law 
                if(u_eff > tiny(one)) then
                    dm2=uf/u_eff*(u_eff-u0)/(uf-u0)
                    dm=max(dm,dm2) ! must be larger than last equilibrium dm
                end if
            end if
        
            ! check dm and update fstat
            if (dm>=dmax) then
                dm=dmax
                fstat=ffailed
            end if
        end if
     
        
        
        ! update sdv
        sdvr(1)=dm
        sdvr(2)=u0
        sdvr(3)=uf
        
      end subroutine FibreCohesiveLaw





      subroutine FibreFailureCriterion(sigma,strength,fstat,findex)
      ! at the moment, only tensile failure is considered
      
        real(dp),                   intent(in) :: sigma(:)
        type(lamina_strength),      intent(in) :: strength
        integer,                   intent(out) :: fstat
        real(dp), optional,        intent(out) :: findex
      
        
      
        ! local variables
        real(dp)    :: Xt, Xc ! tensile, compressive strengths
        real(dp)    :: findex2

        
        ! initialize variables
        if(present(findex)) findex=zero                     ! intent (out)
        Xt=zero; Xc=zero; findex2=zero
        fstat=0
        
      
        ! extract strength parameters
        Xt=strength%Xt
        Xc=strength%Xc
        
        
        ! --------------------------------------------------------- !
        ! the following assumes max stress criterion
        ! other criteria can also be used in the future
        ! just need to put in a selection criterion and algorithm
        ! --------------------------------------------------------- ! 
        ! e.g.: select case (strength%FC)
        !           case(0)
        !               quad. stress
        !           case(1)
        !               max. stress
        !           ...
        
        if(min(Xt,Xc) > tiny(one)) then
            ! failure index for tensile failure; matrix crack perpendicular to shell plane, no z-dir stress components
            findex2=max(sigma(1),zero)/Xt + abs(min(sigma(1),zero)/Xc)
            
            if(findex2>=one) fstat=ffonset
        else
            ! strength close to zero == already failed
            fstat=ffailed
        end if
        
        if(present(findex)) findex=findex2
      
      end subroutine FibreFailureCriterion
      
    

  
      
      subroutine MatrixFailureCriterion(sigma,strength,fstat,findex)
      ! at the moment, only tensile failure is considered
      
        real(dp),                   intent(in) :: sigma(:)
        type(lamina_strength),      intent(in) :: strength
        integer,                   intent(out) :: fstat
        real(dp), optional,        intent(out) :: findex
      
        
      
        ! local variables
        integer     :: nst
        real(dp)    :: Yt,Yc,Sl,St ! tensile, compressive, and shear strengths
        real(dp)    :: findex2

        
        ! initialize variables
        if(present(findex)) findex=zero                     ! intent (out)
        Yt=zero; Yc=zero; Sl=zero; St=zero; findex2=zero
        nst=0; fstat=0
        
        ! extract nst
        nst=size(sigma)
      
        ! extract strength parameters
        Yt=strength%Yt
        Yc=strength%Yc
        Sl=strength%Sl
        St=strength%St
        
        
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
        
        if(min(Yt,St,Sl) > tiny(one)) then
            ! failure index for tensile failure; matrix crack perpendicular to shell plane, no z-dir stress components
            if(nst==3) then 
            findex2=sqrt((max(sigma(2),zero)/Yt)**2 + (sigma(3)/Sl)**2)
            else
            findex2=sqrt((max(sigma(2),zero)/Yt)**2 + (sigma(4)/Sl)**2)
            end if
            
            if(findex2>=one) fstat=mfonset
        else
            ! strength close to zero == already failed
            fstat=mfonset
        end if
        
        if(present(findex)) findex=findex2
      
      end subroutine MatrixFailureCriterion
      
       
      
    end module lamina_type_module
