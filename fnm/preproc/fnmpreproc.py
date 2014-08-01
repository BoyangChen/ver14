
################################################################
####### Preprocessing for 3D linear brick FNM elmt #############
################################################################

from fnmclasses import* # objects defined for FNM
import math


#***************************************************************
#   define input files
#***************************************************************

jobname='Patch1'

abqinputfile=jobname+'.inp'         # abaqus input file
fnminputfile='fnm-'+jobname+'.inp'  # fnm uel input file



#***************************************************************
#   define model information
#***************************************************************

ndim=3          # dimension

nprops=0        # no. of input material properties used in uel code
nsvars=0        # no. of sol. dpdnt var. used and ouput by uel code to be determined



#***************************************************************
#   define material information
#***************************************************************

# material section info
theta='45._dp'  # fibre orientation; change here for other angles

nmat=2          # no. of materials in the model, = sum of no. of the following individual materials
niso=0          # no. of isotropic materials
nlamina=1       # no. of lamina materials
ninterface=1    # no. of cohesive materials
mname=["'bulkmat'","'cohmat'"]
mtype=["'lamina'","'interface'"]
mkey=[1,1]      # index of the two materials in their respective type arrays
bulkmat=1       # index of bulk material in the material array
cohmat=2        # index of cohesive material in the material array

# *** change below for other lamina/cohesive properties ***

# lamina material properties
matlam=[lamina( lamina_modulus(\
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
                                     )      )]

# cohesive material properties
matcoh=[interface(   interface_modulus(\
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
                                       )    )]









#***************************************************************
#   Open Fortran modules to be written during pre-processing
#***************************************************************
lib_mat=open('lib_mat_module.f90','w')    # array of all materials
lib_node=open('lib_node_module.f90','w')  # array of all nodes
lib_edge=open('lib_edge_module.f90','w')  # array of all edges
lib_elem=open('lib_elem_module.f90','w')  # array of all elements






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
        lib_mat.write('        call update(lib_mat('+str(i+1)+'),matname='+mname[i]+',mattype='+mtype[i]+',matkey='+str(mkey[i])+')\n')


# write lamina material properties
if(nlamina>0):
    for i in range(nlamina):
        lib_mat.write('        call update(lib_lamina('+str(i+1)+'), & \n')
        lib_mat.write('      & lamina_modulus(& \n') 
        lib_mat.write('      & E1='                +str(matlam[i].modulus.E1)+               '_dp,& \n')
        lib_mat.write('      & E2='                +str(matlam[i].modulus.E2)+               '_dp,& \n')
        lib_mat.write('      & G12='                +str(matlam[i].modulus.G12)+              '_dp,& \n')
        lib_mat.write('      & G23='                +str(matlam[i].modulus.G23)+              '_dp,& \n')
        lib_mat.write('      & nu12='                +str(matlam[i].modulus.nu12)+             '_dp,& \n')
        lib_mat.write('      & nu23='                +str(matlam[i].modulus.nu23)+            '_dp),& \n')
        lib_mat.write('      & lamina_strength(& \n')
        lib_mat.write('      & Xt='                +str(matlam[i].strength.Xt)+             '_dp,& \n')
        lib_mat.write('      & Xc='                +str(matlam[i].strength.Xc)+              '_dp,& \n')
        lib_mat.write('      & Yt='                +str(matlam[i].strength.Yt)+              '_dp,& \n')
        lib_mat.write('      & Yc='                +str(matlam[i].strength.Yc)+              '_dp,& \n')
        lib_mat.write('      & Sl='                +str(matlam[i].strength.Sl)+              '_dp,& \n')
        lib_mat.write('      & St='                +str(matlam[i].strength.St)+             '_dp),& \n')
        lib_mat.write('      & lamina_matrixtoughness(& \n') 
        lib_mat.write('      & GmcI='                         +str(matlam[i].matrixtoughness.GmcI)+     '_dp,& \n')
        lib_mat.write('      & GmcII='                         +str(matlam[i].matrixtoughness.GmcII)+    '_dp,& \n')
        lib_mat.write('      & eta='                         +str(matlam[i].matrixtoughness.eta)+     '_dp),& \n')
        lib_mat.write('      & lamina_fibretoughness(& \n')  
        lib_mat.write('      & GfcT='                         +str(matlam[i].fibretoughness.GfcT)+      '_dp,& \n')
        lib_mat.write('      & GfcC='                         +str(matlam[i].fibretoughness.GfcC)+      '_dp)) \n')
        lib_mat.write('\n')



# write interface material properties
if(ninterface>0):
    for i in range(ninterface):
        lib_mat.write('        call update(lib_interface('+str(i+1)+'), & \n')
        lib_mat.write('      & interface_modulus(& \n')
        lib_mat.write('      & Dnn='                   +str(matcoh[i].modulus.Dnn)+          '_dp,& \n')
        lib_mat.write('      & Dtt='                   +str(matcoh[i].modulus.Dtt)+          '_dp,& \n')
        lib_mat.write('      & Dll='                   +str(matcoh[i].modulus.Dll)+         '_dp),& \n')
        lib_mat.write('      & interface_strength(& \n')
        lib_mat.write('      & tau_nc='                    +str(matcoh[i].strength.tau_nc)+     '_dp,& \n')
        lib_mat.write('      & tau_tc='                    +str(matcoh[i].strength.tau_tc)+     '_dp,& \n')
        lib_mat.write('      & tau_lc='                    +str(matcoh[i].strength.tau_lc)+    '_dp),& \n')
        lib_mat.write('      & interface_toughness(& \n')
        lib_mat.write('      & Gnc='                     +str(matcoh[i].toughness.Gnc)+      '_dp,& \n')
        lib_mat.write('      & Gtc='                     +str(matcoh[i].toughness.Gtc)+      '_dp,& \n')
        lib_mat.write('      & Glc='                     +str(matcoh[i].toughness.Glc)+      '_dp,& \n')
        lib_mat.write('      & eta='                     +str(matcoh[i].toughness.eta)+      '_dp)) \n')
        lib_mat.write('\n')




#***************************************************************
#       write lib_node_module.f90
#***************************************************************
lib_node.write('    !***************************************! \n')
lib_node.write('    !   the global library of nodes         ! \n')
lib_node.write('    !***************************************! \n')
lib_node.write('                                              \n')
lib_node.write('    include "globals/xnode_module.f90"        \n')
lib_node.write('                                              \n')   
lib_node.write('    module lib_node_module                    \n')
lib_node.write('    use parameter_module                      \n')
lib_node.write('    use xnode_module                          \n')
lib_node.write('                                              \n')   
lib_node.write('    implicit none                             \n')
lib_node.write('    save                                      \n')
lib_node.write('                                              \n')   
lib_node.write('    type(xnode),allocatable :: lib_node(:)    \n')
lib_node.write('                                              \n')   
lib_node.write('    contains                                  \n')
lib_node.write('                                              \n')    
lib_node.write('    subroutine initialize_lib_node()          \n')
lib_node.write('                                              \n')
lib_node.write('        integer :: nnode=0                    \n')   
lib_node.write('        integer :: i=0                        \n')





#***************************************************************
#       write lib_edge_module.f90
#***************************************************************
lib_edge.write('    !***************************************! \n')
lib_edge.write('    !   the global library of edges         ! \n')
lib_edge.write('    !***************************************! \n')
lib_edge.write('                                              \n')    
lib_edge.write('    module lib_edge_module                    \n')
lib_edge.write('    use parameter_module                      \n')
lib_edge.write('                                              \n')
lib_edge.write('    implicit none                             \n')
lib_edge.write('    save                                      \n')
lib_edge.write('                                              \n')
lib_edge.write('    integer,allocatable :: lib_edge(:)        \n')
lib_edge.write('                                              \n')
lib_edge.write('    contains                                  \n')
lib_edge.write('                                              \n')
lib_edge.write('    subroutine initialize_lib_edge()          \n')
lib_edge.write('                                              \n')
lib_edge.write('        integer :: nedge=0                    \n')
lib_edge.write('        integer :: i=0                        \n')
lib_edge.write('                                              \n')







#***************************************************************
#       write lib_elem_module.f90
#***************************************************************
lib_elem.write('    !***************************************!                             \n')
lib_elem.write('    !   the global library of elements      !                             \n')
lib_elem.write('    !***************************************!                             \n')
#if(ndim==2)
lib_elem.write('    include "elements/tri_element_module.f90"                             \n')
lib_elem.write('    include "elements/quad_element_module.f90"                            \n')
lib_elem.write('    include "elements/coh2d_element_module.f90"                           \n')
lib_elem.write('    include "elements/sub2d_element_module.f90"                           \n')
lib_elem.write('    include "elements/xquad_element_module.f90"                           \n')
lib_elem.write('    include "elements/element_module.f90"                                 \n')
#else
lib_elem.write('    include "elements/wedge_element_module.f90"                           \n')
lib_elem.write('    include "elements/brick_element_module.f90"                           \n')
lib_elem.write('    include "elements/coh3d6_element_module.f90"                          \n')
lib_elem.write('    include "elements/coh3d8_element_module.f90"                          \n')
lib_elem.write('    include "elements/sub3d_element_module.f90"                           \n')
lib_elem.write('    include "elements/xbrick_element_module.f90"                          \n')
#end
lib_elem.write('    include "elements/element_module.f90"                                 \n')
lib_elem.write('                                                                          \n')
lib_elem.write('                                                                          \n')
lib_elem.write('    module lib_elem_module                                                \n')
lib_elem.write('    use parameter_module                                                  \n')
#if(ndim==2)
lib_elem.write('    use tri_element_module                                                \n')
lib_elem.write('    use quad_element_module                                               \n')
lib_elem.write('    use coh2d_element_module                                              \n')
lib_elem.write('    use sub2d_element_module                                              \n')
lib_elem.write('    use xquad_element_module                                              \n')
#else
lib_elem.write('    use wedge_element_module                                              \n')
lib_elem.write('    use brick_element_module                                              \n')
lib_elem.write('    use coh3d6_element_module                                             \n')
lib_elem.write('    use coh3d8_element_module                                             \n')
lib_elem.write('    use sub3d_element_module                                              \n')
lib_elem.write('    use xbrick_element_module                                             \n')
#end
lib_elem.write('    use element_module                                                    \n')
lib_elem.write('                                                                          \n')
lib_elem.write('    implicit none                                                         \n')
lib_elem.write('    save                                                                  \n')
lib_elem.write('                                                                          \n')
lib_elem.write('    type(element),          allocatable :: lib_elem(:)                    \n')
#if(ndim==2)     
lib_elem.write('    type(tri_element),      allocatable :: lib_tri(:)                     \n')
lib_elem.write('    type(quad_element),     allocatable :: lib_quad(:)                    \n')
lib_elem.write('    type(coh2d_element),    allocatable :: lib_coh2d(:)                   \n')
lib_elem.write('    type(xquad_element),    allocatable :: lib_xquad(:)                   \n')
#else
lib_elem.write('    type(wedge_element),    allocatable :: lib_wedge(:)                   \n')
lib_elem.write('    type(brick_element),    allocatable :: lib_brick(:)                   \n')
lib_elem.write('    type(coh3d6_element),   allocatable :: lib_coh3d6(:)                  \n')
lib_elem.write('    type(coh3d8_element),   allocatable :: lib_coh3d8(:)                  \n')  
lib_elem.write('    type(xbrick_element),   allocatable :: lib_xbrick(:)                  \n')
#end
lib_elem.write('                                                                          \n')   
lib_elem.write('    contains                                                              \n')
lib_elem.write('                                                                          \n')    
lib_elem.write('    subroutine initialize_lib_elem                                        \n')
lib_elem.write('                                                                          \n') 
lib_elem.write('        integer ::  nelem=0, ntri=0, nquad=0, nwedge=0, nbrick=0 &        \n')
lib_elem.write('        &          ,ncoh2d=0, ncoh3d6=0, ncoh3d8=0, nsub2d=0, nsub3d=0 &  \n')
lib_elem.write('        &          ,nxquad=0, nxbrick=0                                   \n')
lib_elem.write('        integer :: i=0                                                    \n')









#***************************************************************
#       open input files for I/O
#***************************************************************
abqinp=open(abqinputfile,'r')
fnminp=open(fnminputfile,'w')


#***************************************************************
#       define usefule variables
#***************************************************************
nodes=[]
edges=[]
elems=[]
ndedg=[]

#eltype=3080     # just a random name, must be consistent with uel.f
#nndrl=8         # no. of (real)nodes in the element. (here, 8 for linear brick elmt)
#nedge=8         # no. of breakable edges in this element
#nndfl=2*nedge
#nnode=nndrl+nndfl
eltype=[]


lines=abqinp.readlines()
isheader=True
isnode=False
iselem=False
cycle=False

#***************************************************************
#       Read & store nodes and elements, find edges
#***************************************************************
for ln, line in enumerate(lines):
    cycle=False
    
    if(len(line)>=5 and line[0:5]=='*Node'):
        isheader=False
        isnode=True
        cycle=True

    elif(len(line)>=8 and line[0:8]=='*Element'):
        isnode=False
        iselem=True
        cycle=True
        
        if 'C2D3' in line:
            eltype.append('xtri')
        elif 'C2D4' in line:
            eltype.append('xquad')
        elif 'COH2D4' in line:
            eltype.append('coh2d')  # change to x version in future
        elif ('C3D6' in line) or ('SC6' in line):
            eltype.append('xwedge')
        elif ('C3D8' in line) or ('SC8' in line):
            eltype.append('xbrick')
        elif 'COH3D6' in line:
            eltype.append('coh3d6') # change to x version in future
        elif 'COH3D8' in line:
            eltype.append('coh3d8') # change to x version in future
        else
            print 'warning: abaqus element type in line:', ln, 'is not supported in fnm.'

    elif(len(line)>=9 and line[0:9]=='*End Part'):
        iselem=False
      
    if not cycle:
        if isheader:
            fnminp.write(line)
        elif isnode:
        # store nodes first
            l = []
            for t in line.split(','):
                try:
                    l.append(float(t))
                except ValueError:
                    pass

            if ndim==2:
                nodes.append(node(x=l[1], y=l[2], z=0.0))
            else:
                nodes.append(node(x=l[1], y=l[2], z=l[3]))  

            # append the ndedg row to the correct length (nnode)
            ndedg.append([]) 
            
            # append the ndedg col to the correct length (nnode)
            for i in range(len(ndedg)):
                ndedg[i]=[0]*len(ndedg)
            
        elif iselem:
        # build edges, store elements' nodecnc and edgecnc
            l = []
            for t in line.split(','):
                try:
                    l.append(int(t))
                except ValueError:
                    pass
 
            nds=l[1:]
            
            elems.append(element(nodes=nds,edges=[]))
            
            if ndim==2:
            # in 2D, all edges are breakable
                for i in range(len(nds)):
                    row=nds[i]-1
                    
                    if i==len(nds)-1:
                        col=nds[0]-1
                    else:
                        col=nds[i+1]-1
                        
                    if ndedg[row][col]==0:
                    # this pair of nodes hasn't been assigned to an edge
                        fn1=len(nodes)                          # indices of 2 fl. nodes on this new edge
                        fn2=fn1+1
                        for nf in range(2):                     # create floating nodes on this edge
                            nodes.append(node(0.0,0.0,0.0))
                            
                        edges.append(edge(nodes=[row,col,fn1,fn2]))     # create this edge
                        ndedg[row][col]=len(edges)              # fill the new edge index in the ndedg matrix
                        ndedg[col][row]=-(len(edges))           # nodes in rev. order makes the same edge in rev. dir.
                        elems[-1].edges.append(ndedg[row][col]) # append this edge no. in this elem
                    else:
                    # this pair of nodes has been assigned to an edge
                        elems[-1].edges.append(ndedg[row][col]) # append this edge no. in this elem
                        
            else:
            # in 3D FNM for composites, only edges parallel to shell plane are breakable
                for j in range(2):
                    for i in range(len(nds)/2):
                        row=nds[i+j*len(nds)/2]-1
                        
                        if i==len(nds)/2-1:
                            col=nds[j*len(nds)/2]-1
                        else:
                            col=nds[i+1+j*len(nds)/2]-1
                            
                        if ndedg[row][col]==0:
                        # this pair of nodes hasn't been assigned to an edge
                            fn1=len(nodes)                          # indices of 2 fl. nodes on this new edge
                            fn2=fn1+1
                            for nf in range(2):                     # create floating nodes on this edge
                                nodes.append(node(0.0,0.0,0.0))
                                
                            edges.append(edge(nodes=[row,col,fn1,fn2]))     # create this edge
                            ndedg[row][col]=len(edges)              # fill the new edge index in the ndedg matrix
                            ndedg[col][row]=-(len(edges))           # nodes in rev. order makes the same edge in rev. dir.
                            elems[-1].edges.append(ndedg[row][col]) # append this edge no. in this elem
                        else:
                        # this pair of nodes has been assigned to an edge
                            elems[-1].edges.append(ndedg[row][col]) # append this edge no. in this elem            
            
        else:
            break
            #fnminp.write(line)


#***************************************************************        
#       Write Nodes
#***************************************************************
fnminp.write('*Node\n')
lib_node.write('        nnode='+str(len(nodes))+'   \n')
lib_node.write('        allocate(lib_node(nnode))   \n')

for cntr0, nd in enumerate(nodes):
    cntr=cntr0+1
    if ndim==2:
        fnminp.write(str(cntr)+', '+str(nd.x)+', '+str(nd.y)+'\n')
        lib_node.write('        call update(lib_node('+str(cntr)+'),x=['+str(nd.x)+','+str(nd.y)+'],u=[zero,zero])\n')
    else:
        fnminp.write(str(cntr)+', '+str(nd.x)+', '+str(nd.y)+', '+str(nd.z)+'\n')
        lib_node.write('        call update(lib_node('+str(cntr)+'),x=['+str(nd.x)+','+str(nd.y)+','+str(nd.z)+'],u=[zero,zero,zero])\n')


#***************************************************************        
#       Write Edges
#***************************************************************
lib_edge.write('        nedge='+str(len(edges))+'   \n')
lib_edge.write('        allocate(lib_edge(nedge))   \n')
lib_edge.write('        lib_edge=0                  \n')

    
#***************************************************************        
#       Write Elements
#***************************************************************    
fnminp.write('*USER ELEMENT, TYPE='+str(eltype)+', NODES='+str(nnode)+', COORDINATES='+str(ndim)+
    ', PROPERTIES='+str(nprops)+', VARIABLES='+str(nsvars)+'\n')
if ndim==2:
    fnminp.write('1,2\n')
else:
    fnminp.write('1,2,3\n')
      
fnminp.write('*Element, type='+str(eltype)+', elset=uel\n')

lib_elem.write('        nxbrick='+str(len(elems))+'     \n')
lib_elem.write('        nelem=nxbrick                   \n')
lib_elem.write('        allocate(lib_xbrick(nxbrick))   \n')
lib_elem.write('        allocate(lib_elem(nelem))       \n')
 

for cntr0, el in enumerate(elems):
    cntr=cntr0+1
    eline=str(cntr)
    nodecnc=''
    edgecnc=''
    for k in el.nodes:
        eline=eline+', '+str(k)
        nodecnc=nodecnc+str(k)+','
        
    for m in el.edges:
        edgecnc=edgecnc+str(abs(m))+','
        if m>0:
            eline=eline+', '+str(edges[m-1].nodes[2])+', '+str(edges[m-1].nodes[3])
            nodecnc=nodecnc+str(edges[m-1].nodes[2])+', '+str(edges[m-1].nodes[3])+','
        elif m<0:
            eline=eline+', '+str(edges[-m-1].nodes[3])+', '+str(edges[-m-1].nodes[2])
            nodecnc=nodecnc+str(edges[-m-1].nodes[3])+', '+str(edges[-m-1].nodes[2])+','
        else:
            print 'warning: edges in elem ',cntr,' are empty'
    
    eline=eline+'\n'
    fnminp.write(eline)
    
    lib_elem.write('        call prepare(lib_xbrick('+str(cntr)+'),key='+str(cntr)+', & \n')
    lib_elem.write('& nodecnc=['+nodecnc[:-1]+'], & \n')
    lib_elem.write('& edgecnc=['+edgecnc[:-1]+'], & \n')
    lib_elem.write('& bulkmat='+str(bulkmat)+', cohmat='+str(cohmat)+', theta='+theta+' ) \n')
    
    lib_elem.write('        call update(lib_elem('+str(cntr)+'),elname="xbrick",eltype="xbrick",elkey='+str(cntr)+') \n')
    lib_elem.write('\n')


#***************************************************************        
#       Write the rest
#***************************************************************

for line in lines[ln:]:
    fnminp.write(line)




#   close lib_mat_module.f90
lib_mat.write('    end subroutine initialize_lib_mat          \n')
lib_mat.write('    end module lib_mat_module                  \n')
lib_mat.close()
#   close lib_node_module.f90
lib_node.write('    end subroutine initialize_lib_node        \n')
lib_node.write('    end module lib_node_module                \n')
lib_node.close()
#   close lib_node_module.f90
lib_edge.write('    end subroutine initialize_lib_edge        \n')
lib_edge.write('    end module lib_edge_module                \n')
lib_edge.close()
#   close lib_elem_module.f90
lib_elem.write('    end subroutine initialize_lib_elem        \n')
lib_elem.write('    end module lib_elem_module                \n')
lib_elem.close()


