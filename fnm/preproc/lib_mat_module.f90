    !***************************************!                  
    !   the global library of materials     !                  
    !***************************************!                  
                                                               
    include "materials/isotropic_type_module.f90"              
    include "materials/lamina_type_module.f90"                 
    include "materials/interface_type_module.f90"              
    include "materials/material_module.f90"                    
                                                               
    module lib_mat_module                                      
    use parameter_module                                       
    use isotropic_type_module                                  
    use lamina_type_module                                     
    use interface_type_module                                  
    use material_module                                        
                                                               
    implicit none                                              
    save                                                       
                                                               
    type(material),       allocatable    :: lib_mat(:)         
    type(isotropic_type), allocatable    :: lib_iso(:)         
    type(lamina_type),    allocatable    :: lib_lamina(:)      
    type(interface_type), allocatable    :: lib_interface(:)   
                                                               
    contains                                                   
                                                               
    subroutine initialize_lib_mat()                            
                                                               
        integer :: i=0, nmat=0, niso=0, nlamina=0, ninterface=0
        nmat=2                                            
        niso=0                                            
        nlamina=1                                      
        ninterface=1                                
        if(nmat>0) allocate(lib_mat(nmat))                       
        if(niso>0) allocate(lib_iso(niso))                       
        if(nlamina>0) allocate(lib_lamina(nlamina))              
        if(ninterface>0) allocate(lib_interface(ninterface))     
        call update(lib_mat(1),matname='bulkmat',mattype='lamina',matkey=1)
        call update(lib_mat(2),matname='cohmat',mattype='interface',matkey=1)
        call update(lib_lamina(1), & 
      & lamina_modulus(& 
      & E1=161000.0_dp,& 
      & E2=11400.0_dp,& 
      & G12=5170.0_dp,& 
      & G23=3980.0_dp,& 
      & nu12=0.34_dp,& 
      & nu23=0.43_dp),& 
      & lamina_strength(& 
      & Xt=2806.0_dp,& 
      & Xc=1400.0_dp,& 
      & Yt=60.0_dp,& 
      & Yc=185.0_dp,& 
      & Sl=90.0_dp,& 
      & St=90.0_dp),& 
      & lamina_matrixtoughness(& 
      & GmcI=0.2_dp,& 
      & GmcII=1.0_dp,& 
      & eta=1.0_dp),& 
      & lamina_fibretoughness(& 
      & GfcT=100.0_dp,& 
      & GfcC=100.0_dp)) 

        call update(lib_interface(1), & 
      & interface_modulus(& 
      & Dnn=1000000.0_dp,& 
      & Dtt=1000000.0_dp,& 
      & Dll=1000000.0_dp),& 
      & interface_strength(& 
      & tau_nc=60.0_dp,& 
      & tau_tc=90.0_dp,& 
      & tau_lc=90.0_dp),& 
      & interface_toughness(& 
      & Gnc=0.293_dp,& 
      & Gtc=0.631_dp,& 
      & Glc=0.631_dp,& 
      & eta=1.0_dp)) 

    end subroutine initialize_lib_mat          
    end module lib_mat_module                  
