################################################################
####### Preprocessing for writing FNM lib_mat module ###########
################################################################

from fnmclasses import* # glb objects defined for FNM
import math


#***************************************************************
#   define material information
#***************************************************************

# create a library of materials
list_mat=[]     # len(mat)=nmat
list_iso=[]
list_lamina=[]
list_interface=[]

# *** change below for other lamina/cohesive properties ***

# IM7/8552 UD carbon epoxy lamina material properties
IM7_8552_UD=lamina( lamina_modulus(\
                                        E1=161000., 
                                        E2=11400., 
                                        G12=5170., 
                                        G23=3980., 
                                        nu12=0.34, 
                                        nu23=0.43   
                               ),
                lamina_strength(\
                                        Xt=2806., 
                                        Xc=1400., 
                                        Yt=60., 
                                        Yc=185., 
                                        Sl=90., 
                                        St=90.     
                                ),
                lamina_matrixtoughness(\
                                        GmcI=0.2, 
                                        GmcII=1., 
                                        eta=1.
                                       ),
                lamina_fibretoughness(\
                                        GfcT=100., 
                                        GfcC=100.   
                                     )  )

# IM7/8552 matrix crack cohesive material properties
IM7_8552_Matrix=interface(   interface_modulus(\
                                        Dnn=1000000., 
                                        Dtt=1000000., 
                                        Dll=1000000.
                                     ),
                    interface_strength(\
                                        tau_nc=60., 
                                        tau_tc=90., 
                                        tau_lc=90.
                                      ),
                    interface_toughness(\
                                        Gnc=0.293, 
                                        Gtc=0.631, 
                                        Glc=0.631, 
                                        eta=1.
                                       )    )


list_lamina.append(IM7_8552_UD)
list_interface.append(IM7_8552_Matrix)

list_mat.append(material(name="'IM7-8552 0 ply'",          type="'lamina'",      typekey=1, theta=0.0))
list_mat.append(material(name="'IM7-8552 45 ply'",         type="'lamina'",      typekey=1, theta=45.0))
list_mat.append(material(name="'IM7-8552 90 ply'",         type="'lamina'",      typekey=1, theta=90.0))
list_mat.append(material(name="'IM7-8552 -45 ply'",        type="'lamina'",      typekey=1, theta=-45.0))
list_mat.append(material(name="'IM7-8552 Matrix Crack'",   type="'interface'",   typekey=1, theta=0.0))

nmat=len(list_mat)             # no. of materials in the model, = sum of no. of the following individual materials
niso=len(list_iso)             # no. of isotropic materials
nlamina=len(list_lamina)       # no. of lamina materials
ninterface=len(list_interface) # no. of cohesive materials

#***************************************************************
#   Open Fortran modules to be written during pre-processing
#***************************************************************
lib_mat=open('lib_mat_module.f90','w')    # array of all materials


#***************************************************************
#       write lib_mat_module.f90
#***************************************************************
lib_mat.write('    !***************************************!                  \n')
lib_mat.write('    !   the global library of materials     !                  \n')
lib_mat.write('    !***************************************!                  \n')
lib_mat.write('                                                               \n')
lib_mat.write('    include "materials/isotropic_type_module.f90"              \n')
lib_mat.write('    include "materials/lamina_type_module.f90"                 \n')
lib_mat.write('    include "materials/interface_type_module.f90"              \n')
lib_mat.write('    include "materials/material_module.f90"                    \n')
lib_mat.write('                                                               \n')
lib_mat.write('    module lib_mat_module                                      \n')
lib_mat.write('    use parameter_module                                       \n')
lib_mat.write('    use isotropic_type_module                                  \n')
lib_mat.write('    use lamina_type_module                                     \n')
lib_mat.write('    use interface_type_module                                  \n')
lib_mat.write('    use material_module                                        \n')
lib_mat.write('                                                               \n')
lib_mat.write('    implicit none                                              \n')
lib_mat.write('    save                                                       \n')
lib_mat.write('                                                               \n')
lib_mat.write('    type(material),       allocatable    :: lib_mat(:)         \n')
lib_mat.write('    type(isotropic_type), allocatable    :: lib_iso(:)         \n')
lib_mat.write('    type(lamina_type),    allocatable    :: lib_lamina(:)      \n')
lib_mat.write('    type(interface_type), allocatable    :: lib_interface(:)   \n')
lib_mat.write('                                                               \n')
lib_mat.write('    contains                                                   \n')
lib_mat.write('                                                               \n')
lib_mat.write('    subroutine initialize_lib_mat()                            \n')
lib_mat.write('                                                               \n')
lib_mat.write('        integer :: i=0, nmat=0, niso=0, nlamina=0, ninterface=0\n')

lib_mat.write('        nmat='+str(nmat)+'                                            \n')
lib_mat.write('        niso='+str(niso)+'                                            \n')
lib_mat.write('        nlamina='+str(nlamina)+'                                      \n')
lib_mat.write('        ninterface='+str(ninterface)+'                                \n')
lib_mat.write('        if(nmat>0) allocate(lib_mat(nmat))                       \n')
lib_mat.write('        if(niso>0) allocate(lib_iso(niso))                       \n')
lib_mat.write('        if(nlamina>0) allocate(lib_lamina(nlamina))              \n')
lib_mat.write('        if(ninterface>0) allocate(lib_interface(ninterface))     \n')

# write material section info
if(nmat>0):
    for i in range(nmat):
        lib_mat.write('        call update(lib_mat('+str(i+1)+'),matname='+list_mat[i].name+\
        ',mattype='+list_mat[i].type+',typekey='+str(list_mat[i].typekey)+',theta='+str(list_mat[i].theta)+'_dp'+')\n')


# write lamina material properties
if(nlamina>0):
    for i in range(nlamina):
        lib_mat.write('        call update(lib_lamina('+str(i+1)+'), & \n')
        lib_mat.write('      & lamina_modulus(& \n') 
        lib_mat.write('      & E1='                +str(list_lamina[i].modulus.E1)+               '_dp,& \n')
        lib_mat.write('      & E2='                +str(list_lamina[i].modulus.E2)+               '_dp,& \n')
        lib_mat.write('      & G12='                +str(list_lamina[i].modulus.G12)+              '_dp,& \n')
        lib_mat.write('      & G23='                +str(list_lamina[i].modulus.G23)+              '_dp,& \n')
        lib_mat.write('      & nu12='                +str(list_lamina[i].modulus.nu12)+             '_dp,& \n')
        lib_mat.write('      & nu23='                +str(list_lamina[i].modulus.nu23)+            '_dp),& \n')
        lib_mat.write('      & lamina_strength(& \n')
        lib_mat.write('      & Xt='                +str(list_lamina[i].strength.Xt)+             '_dp,& \n')
        lib_mat.write('      & Xc='                +str(list_lamina[i].strength.Xc)+              '_dp,& \n')
        lib_mat.write('      & Yt='                +str(list_lamina[i].strength.Yt)+              '_dp,& \n')
        lib_mat.write('      & Yc='                +str(list_lamina[i].strength.Yc)+              '_dp,& \n')
        lib_mat.write('      & Sl='                +str(list_lamina[i].strength.Sl)+              '_dp,& \n')
        lib_mat.write('      & St='                +str(list_lamina[i].strength.St)+             '_dp),& \n')
        lib_mat.write('      & lamina_fibretoughness(& \n')  
        lib_mat.write('      & GfcT='                         +str(list_lamina[i].fibretoughness.GfcT)+      '_dp,& \n')
        lib_mat.write('      & GfcC='                         +str(list_lamina[i].fibretoughness.GfcC)+      '_dp),& \n')
        # matrix toughness is not needed in lamina type, as matrix crack is treated by a cohesive sub-element;
        # matrix crack smeared crack model is not supported here in fnm lamina type
        #lib_mat.write('      & lamina_matrixtoughness(& \n') 
        #lib_mat.write('      & GmcI='                         +str(list_lamina[i].matrixtoughness.GmcI)+     '_dp,& \n')
        #lib_mat.write('      & GmcII='                         +str(list_lamina[i].matrixtoughness.GmcII)+    '_dp,& \n')
        #lib_mat.write('      & eta='                         +str(list_lamina[i].matrixtoughness.eta)+     '_dp),& \n')
        lib_mat.write('      & ) \n')
        lib_mat.write('\n')



# write interface material properties
if(ninterface>0):
    for i in range(ninterface):
        lib_mat.write('        call update(lib_interface('+str(i+1)+'), & \n')
        lib_mat.write('      & interface_modulus(& \n')
        lib_mat.write('      & Dnn='                   +str(list_interface[i].modulus.Dnn)+          '_dp,& \n')
        lib_mat.write('      & Dtt='                   +str(list_interface[i].modulus.Dtt)+          '_dp,& \n')
        lib_mat.write('      & Dll='                   +str(list_interface[i].modulus.Dll)+         '_dp),& \n')
        lib_mat.write('      & interface_strength(& \n')
        lib_mat.write('      & tau_nc='                    +str(list_interface[i].strength.tau_nc)+     '_dp,& \n')
        lib_mat.write('      & tau_tc='                    +str(list_interface[i].strength.tau_tc)+     '_dp,& \n')
        lib_mat.write('      & tau_lc='                    +str(list_interface[i].strength.tau_lc)+    '_dp),& \n')
        lib_mat.write('      & interface_toughness(& \n')
        lib_mat.write('      & Gnc='                     +str(list_interface[i].toughness.Gnc)+      '_dp,& \n')
        lib_mat.write('      & Gtc='                     +str(list_interface[i].toughness.Gtc)+      '_dp,& \n')
        lib_mat.write('      & Glc='                     +str(list_interface[i].toughness.Glc)+      '_dp,& \n')
        lib_mat.write('      & eta='                     +str(list_interface[i].toughness.eta)+      '_dp)) \n')
        lib_mat.write('\n')



#   close lib_mat_module.f90
lib_mat.write('    end subroutine initialize_lib_mat          \n')
lib_mat.write('    end module lib_mat_module                  \n')
lib_mat.close()